(*{{{ Copyright (c) 2014 Andy Ray
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
  }}}*)

open Js_of_ocaml
module C = Cohttp
module CLB = Cohttp_eio.Body

module type Params = sig
  val convert_body_string : Js.js_string Js.t -> string
  val with_credentials : bool
end

let xhr_response_supported =
  (* from http://stackoverflow.com/questions/8926505/how-to-feature-detect-if-xmlhttprequest-supports-responsetype-arraybuffer *)
  lazy
    (let xhr = XmlHttpRequest.create () in
     let rt = xhr##.responseType in
     Js.to_string (Js.typeof rt) = "string")

let binary_string str =
  let len = String.length str in
  let a = new%js Typed_array.uint8Array len in
  for i = 0 to len - 1 do
    Typed_array.set a i (Char.code str.[i])
  done;
  a

let string_of_uint8array u8a offset len =
  String.init len (fun i -> Char.chr (Typed_array.unsafe_get u8a (offset + i)))

module IO = Cohttp_eio.Private.IO
module Header_io = Cohttp.Private.Header_io.Make (IO)

module Body_builder (P : Params) = struct
  let src = Logs.Src.create "cohttp.eio.jsoo" ~doc:"Cohttp Eio JSOO module"

  module Log = (val Logs.src_log src : Logs.LOG)

  (* choose between chunked and direct transfer *)
  let get = function
    | `String js_str -> CLB.of_string (P.convert_body_string js_str)
    | `ArrayBuffer ab ->
        let u8a = new%js Typed_array.uint8Array_fromBuffer ab in
        CLB.of_string (string_of_uint8array u8a 0 ab##.byteLength)

  let construct_body xml =
    (* construct body *)
    let b =
      let respText () =
        Js.Opt.case xml##.responseText
          (fun () -> `String (Js.string ""))
          (fun s -> `String s)
      in
      match Lazy.force xhr_response_supported with
      | true when Js.Opt.return xml##.response == Js.null ->
          Log.warn (fun m -> m "XHR Response is null; using empty string");
          `String (Js.string "")
      | true ->
          Js.Opt.case
            (File.CoerceTo.arrayBuffer xml##.response)
            (fun () ->
              Log.warn (fun m ->
                  m "XHR Response is not an arrayBuffer; using responseText");
              respText ())
            (fun ab -> `ArrayBuffer ab)
      | false -> respText ()
    in
    get b
end

module type S =
  Cohttp.Generic.Client.S
    with type 'a with_context = unit -> sw:Eio.Std.Switch.t -> 'a
     and type 'a io = 'a
     and type body = Cohttp_eio.Body.t

module Make_api (X : sig
  val call :
    ?headers:Http.Header.t ->
    ?body:Cohttp_eio.Body.t ->
    Http.Method.t ->
    Uri.t ->
    Http.Response.t * Cohttp_eio.Body.t
end) : S = struct
  type 'a io = 'a
  type body = Cohttp_eio.Body.t
  type 'a with_context = unit -> sw:Eio.Std.Switch.t -> 'a

  let map_context v f client ~sw = f (v client ~sw)

  let call () ~sw:_ ?headers ?body ?chunked:_ meth uri =
    X.call ?headers ?body meth uri

  (* The HEAD should not have a response body *)
  let head () ~sw ?headers uri =
    fst (call () ~sw ?headers ~chunked:false `HEAD uri)

  let get () ~sw ?headers uri = call () ~sw ?headers ~chunked:false `GET uri

  let delete () ~sw ?body ?chunked ?headers uri =
    call () ~sw ?headers ?body ?chunked `DELETE uri

  let post () ~sw ?body ?chunked ?headers uri =
    call () ~sw ?headers ?body ?chunked `POST uri

  let put () ~sw ?body ?chunked ?headers uri =
    call () ~sw ?headers ?body ?chunked `PUT uri

  let patch () ~sw ?body ?chunked ?headers uri =
    call () ~sw ?headers ?body ?chunked `PATCH uri
end

module Make_client_async (P : Params) = Make_api (struct
  module Bb = Body_builder (P)

  let call ?headers ?body meth uri =
    Eio_js_backend.await
      ~setup:(fun ~resolve ~reject:_ ->
        let xml = XmlHttpRequest.create () in
        xml##.withCredentials := Js.bool P.with_credentials;
        if Lazy.force xhr_response_supported then
          xml##.responseType := Js.string "arraybuffer";
        let () =
          xml
          ## (_open
                (Js.string (C.Code.string_of_method meth))
                (Js.string (Uri.to_string uri))
                Js._true)
          (* asynchronous call *)
        in
        (* set request headers *)
        let () =
          match headers with
          | None -> ()
          | Some headers ->
              C.Header.iter
                (fun k v ->
                  (* some headers lead to errors in the javascript console, should
                     we filter then out here? *)
                  xml ## (setRequestHeader (Js.string k) (Js.string v)))
                headers
        in

        xml##.onreadystatechange :=
          Js.wrap_callback (fun _ ->
              match xml##.readyState with
              | XmlHttpRequest.DONE ->
                  let body = Bb.construct_body xml in
                  (* Note; a type checker subversion seems to be possible here (4.01.0).
                   * Remove the type constraint on Lwt.task above and return any old
                   * guff here.  It'll compile and crash in the browser! *)
                  (* (re-)construct the response *)
                  let resp_headers = Js.to_string xml##getAllResponseHeaders in
                  let channel =
                    Eio.Buf_read.of_flow ~max_size:max_int
                      (Eio.Flow.string_source resp_headers)
                  in
                  let response =
                    let resp_headers = Header_io.parse channel in
                    Cohttp.Response.make ~version:`HTTP_1_1
                      ~status:(C.Code.status_of_code xml##.status)
                      ~flush:false (* ??? *)
                      ~headers:resp_headers ()
                  in
                  resolve (response, body)
              | _ -> ());

        (* perform call *)
        (match body with
        | None -> xml ## (send Js.null)
        | Some body ->
            let body = Eio.Flow.read_all body in
            let bs = binary_string body in
            xml##send (Js.Opt.return (Obj.magic bs)));
        xml)
      ~cancel:(fun xml -> xml##abort)
end)

module Make_client_sync (P : Params) = Make_api (struct
  module Response = Cohttp.Response
  module Request = Cohttp.Request
  module Bb = Body_builder (P)

  let call ?headers ?body meth uri =
    let xml = XmlHttpRequest.create () in
    xml##.withCredentials := Js.bool P.with_credentials;
    if Lazy.force xhr_response_supported then
      xml##.responseType := Js.string "arraybuffer";
    let () =
      xml
      ## (_open
            (Js.string (C.Code.string_of_method meth))
            (Js.string (Uri.to_string uri))
            Js._false)
      (* synchronous call *)
    in
    (* set request headers *)
    let () =
      match headers with
      | None -> ()
      | Some headers ->
          C.Header.iter
            (fun k v ->
              (* some headers lead to errors in the javascript console, should
                 we filter then out here? *)
              xml ## (setRequestHeader (Js.string k) (Js.string v)))
            headers
    in
    (* perform call *)
    (match body with
    | None -> xml ## (send Js.null)
    | Some body ->
        let body = Eio.Flow.read_all body in
        let bs = binary_string body in
        xml ## (send (Js.Opt.return (Obj.magic bs))));
    let body = Bb.construct_body xml in
    (* (re-)construct the response *)
    let resp_headers = Js.to_string xml##getAllResponseHeaders in
    let channel =
      Eio.Buf_read.of_flow ~max_size:max_int
        (Eio.Flow.string_source resp_headers)
    in
    let resp_headers = Header_io.parse channel in
    let response =
      Response.make ~version:`HTTP_1_1
        ~status:(Cohttp.Code.status_of_code xml##.status)
        ~flush:false ~headers:resp_headers ()
    in
    (response, body)
end)

module Client = Make_client_async (struct
  let convert_body_string = Js.to_bytestring
  let with_credentials = false
end)

module Client_sync = Make_client_sync (struct
  let convert_body_string = Js.to_bytestring
  let with_credentials = false
end)
