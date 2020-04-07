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
type old_time = string
type error = string
type p_time = Ptime.t * Ptime.tz_offset_s option

(** the name doesn't make much sense anymore, but is kept for compatibility reasons
  * if string is received in rfc3339 format, timezone will be preserved between to/from string
    but not once converted to a float
*)
type iso8601 =
  | P_time of p_time        (* rfc3339 - accepts only the date in YYYY-MM-DD form *)
  | Old of old_time * error (* iso8601 - accepts both YYYYMMDD & YYYY-MM-DD *)

 let of_string x =
  let r = Ptime.of_rfc3339 x in
  match r with
  | Ok (t, tz, _) -> P_time (t, tz) (** prefer to use ptime
                                      * it will only accept dashes *)
  | Error e -> match Ptime.rfc3339_error_to_msg r with
               | Error (`Msg err_msg) -> Old (x, err_msg)
               | Ok x -> failwith "impossible" (* ptime providing bad api *)

let to_string = function
  | Old (x, _)     -> x
  | P_time (t, tz) -> Ptime.to_rfc3339 ?tz_offset_s:tz t

let of_float x =
  let time = Unix.gmtime x in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (time.Unix.tm_year+1900)
    (time.Unix.tm_mon+1)
    time.Unix.tm_mday
    time.Unix.tm_hour
    time.Unix.tm_min
    time.Unix.tm_sec |> of_string

(* Convert tm in localtime to calendar time, x *)
let to_float_localtime x =
  let to_float y mon d h min s =
    fst Unix.(mktime { tm_year = y - 1900;
                       tm_mon = mon - 1;
                       tm_mday = d;
                       tm_hour = h;
                       tm_min = min;
                       tm_sec = s;
                       (* These are ignored: *)
                       tm_wday = 0; tm_yday = 0; tm_isdst = true;
                     })
  in
  match x with
  | P_time (t, tz) ->
    let ((y, mon, d), ((h, min, s), _)) = Ptime.to_date_time ?tz_offset_s:tz t in
    to_float y mon d h min s
  | Old (x, _) -> begin
      try
        Scanf.sscanf x "%04d%02d%02dT%02d:%02d:%02d" (fun y mon d h min s ->
          to_float y mon d h min s
        )
      with e -> invalid_arg (Printf.sprintf "date.ml:to_float_localtime: %s" x)
    end

(* Convert tm in UTC back into calendar time x (using offset between above
   UTC and localtime fns to determine offset between UTC and localtime, then
   correcting for this)
*)
let to_float x =
  let t = Unix.time () in
  let offset = (t |> of_float |> to_float_localtime) -. t in
  to_float_localtime x -. offset

let assert_utc = function
  | Old (x, _)    -> begin
                       try Scanf.sscanf x "%_[0-9]T%_[0-9]:%_[0-9]:%_[0-9]Z" ()
                       with _ -> invalid_arg (Printf.sprintf "date.ml:assert_utc: %s" x)
                     end
  | P_time _ -> ()

let never = of_float 0.0

let eq x y = match x, y with
  | Old _, P_time _ | P_time _, Old _-> false
  | P_time x, P_time y               -> x = y
  | Old (x, _), Old (y, _)           -> x = y
