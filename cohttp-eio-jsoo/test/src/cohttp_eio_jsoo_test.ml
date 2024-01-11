module Client = Cohttp_eio_jsoo.Client
module Js = Js_of_ocaml.Js

let _Promise = Js.Unsafe.global##._Promise

let promise_of_eio eio =
  new%js _Promise
    (Js.wrap_callback (fun resolve reject ->
         Eio_js_backend.start @@ fun () ->
         try
           let res = eio () in
           Js.Unsafe.fun_call resolve [| Js.Unsafe.inject res |]
         with e ->
           let msg = Printexc.to_string e in
           Js.Unsafe.fun_call reject
             [| Js.Unsafe.inject (new%js Js.error_constr (Js.string msg)) |]))

let () =
  Js.export_all
    (object%js
       method request uri =
         let f () =
           let uri = Uri.of_string (Js.to_string uri) in
           Eio.Switch.run @@ fun sw ->
           let response, body = Client.get () ~sw uri in
           let body = Eio.Flow.read_all body in
           let status =
             Http.Response.status response |> Cohttp.Code.code_of_status
           in
           Js.array
             [| Js.Unsafe.inject status; Js.Unsafe.inject @@ Js.string body |]
         in
         promise_of_eio f
    end)
