type success_outcome =
  | Installed
  | FailedConfig
  | NotInstalled
  | HalfInstalled
  | ConfigFiles
  | Unpacked

type error_outcome =
  | Incomplete
  | Timeout

type outcome = (success_outcome, error_outcome) result

type name =
  | Installation

let name_to_string = function
  | Installation -> "Installation"
