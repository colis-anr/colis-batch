include Unix

let pp_time fmt tm =
  let open Unix in
  let tm = gmtime tm in
  Format.fprintf fmt "%4d-%02d-%02d %02d:%02d:%02d"
    (1900 + tm.tm_year) (1 + tm.tm_mon) tm.tm_mday
    tm.tm_hour tm.tm_min tm.tm_sec
