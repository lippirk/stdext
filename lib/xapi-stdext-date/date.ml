(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* ==== RFC822 ==== *)
type rfc822 = string

let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; 
                "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let rfc822_of_float x =
  let time = Unix.gmtime x in
  Printf.sprintf "%s, %d %s %d %02d:%02d:%02d GMT"
    days.(time.Unix.tm_wday) time.Unix.tm_mday
    months.(time.Unix.tm_mon) (time.Unix.tm_year+1900)
    time.Unix.tm_hour time.Unix.tm_min time.Unix.tm_sec

let rfc822_to_string x = x

(* ==== ISO8601/RFC3339 ==== *)

type zone = UTC | Local
type iso8601 = Ptime.t * zone

let of_string x =
  let x =
    try
      (* if x doesn't contain dashes, insert them, so that ptime can parse x *)
      Scanf.sscanf x "%04d%02d%02dT%s" (fun y mon d rest ->
        Printf.sprintf "%04d-%02d-%02dT%s" y mon d rest
      )
    with _ -> x
  in
  match x |> Ptime.of_rfc3339 |> Ptime.rfc3339_error_to_msg with
  | Error (`Msg e) -> invalid_arg (Printf.sprintf "date.ml:of_string: %s" e)
  | Ok (t, tz, _)  -> match tz with
                      | None | Some 0 -> (t, UTC)
                      | Some _        -> invalid_arg (Printf.sprintf "date.ml:of_string: %s" x)

let to_string (t, zone) =
  let t_string =
    (* to ensure Z printed, rather than +00:00 *)
    Ptime.to_rfc3339 ~tz_offset_s:0 t
  in
  match zone with
  (* in all cases, remove '-' for backwards compatibility. for localtime we must also remove 'Z' *)
  | UTC   -> Astring.String.filter (function '-' -> false | _ -> true) t_string
  | Local -> Astring.String.filter (function '-' | 'Z' -> false | _ -> true) t_string

let of_float s =
  let t = match Ptime.of_float_s s with
          | None -> invalid_arg (Printf.sprintf "date.ml:of_float: %f" s)
          | Some t -> t
  in
  (t, UTC)

let _offset_by t offset =
  let offset_int = match offset with None -> 0 | Some i -> i in
  let span = Ptime.Span.of_int_s offset_int in
  match Ptime.add_span t span with
  | None ->
      invalid_arg
        (Printf.sprintf
           "date.ml:localtime: failed to adjust time (%s) with offset (%i)"
           (Ptime.to_rfc3339 t) offset_int)
  | Some now_local ->
      now_local

let localtime () =
  (_offset_by (Ptime_clock.now ()) (Ptime_clock.current_tz_offset_s ()), Local)

(* Convert tm in UTC back into calendar time x (using offset between above
   UTC and localtime fns to determine offset between UTC and localtime, then
   correcting for this)
*)
let to_float (t, zone) =
  match zone with
  | UTC -> Ptime.to_float_s t
  | Local ->
    invalid_arg (Printf.sprintf "date.ml:to_float: expected UTC zone but got Local: %s" (Ptime.to_rfc3339 ~tz_offset_s:0 t))


let assert_utc _ = ()

let never = of_float 0.0

let eq (t_x, o_x) (t_y, o_y) = Ptime.equal t_x t_y && o_x = o_y
