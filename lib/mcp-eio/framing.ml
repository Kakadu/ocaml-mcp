(** Message framing utilities for newline-delimited JSON.

    This module handles the serialization and deserialization of JSON-RPC
    messages using newline-delimited JSON (NDJSON) format. It provides
    conversion between different JSON representations and I/O operations. *)

open Eio

(* Convert between jsonrpc's Json.t and Yojson.Safe.t.
   In modern jsonrpc (>= 1.26) [Json.t] is defined as [Yojson.Safe.t], so these
   are the identity. In older jsonrpc, [Json.t] and [Yojson.Safe.t] share the
   same constructor set, so a plain type coercion is accepted by the type
   checker on both. This keeps the code compiling across jsonrpc/yojson
   versions without enumerating the version-specific constructors. *)
let json_to_yojson (json : Jsonrpc.Json.t) : Yojson.Safe.t = (json :> Yojson.Safe.t)

let yojson_to_json (yojson : Yojson.Safe.t) : Jsonrpc.Json.t = (yojson :> Jsonrpc.Json.t)

let write_packet (sink : _ Flow.sink) (packet : Jsonrpc.Packet.t) =
  let json = Jsonrpc.Packet.yojson_of_t packet in
  let yojson = json_to_yojson json in
  let content = Yojson.Safe.to_string yojson in
  (* Write as newline-delimited JSON *)
  Flow.write sink [ Cstruct.of_string content; Cstruct.of_string "\n" ]

let rec read_packet (reader : Buf_read.t) : Jsonrpc.Packet.t option =
  try
    (* Read a single line of JSON *)
    let line = Buf_read.line reader in
    (* Remove trailing \r if present (in case of \r\n line endings) *)
    let line =
      if String.length line > 0 && line.[String.length line - 1] = '\r' then
        String.sub line 0 (String.length line - 1)
      else line
    in
    (* Skip empty lines *)
    if String.length line = 0 then read_packet reader
    else
      let yojson = Yojson.Safe.from_string line in
      let json = yojson_to_json yojson in
      Some (Jsonrpc.Packet.t_of_yojson json)
  with
  | End_of_file -> None
  | exn ->
      failwith
        (Printf.sprintf "Packet parse error: %s" (Printexc.to_string exn))
