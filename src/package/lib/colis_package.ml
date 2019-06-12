module Package = Package
module Maintscript = Maintscript

type package = Package.t

let parse_package = Package.parse

let map_all_scenarii f = List.map f Scenarii.all
let run_scenario = ScenarioEngine.run

let generate_and_write_html_report = HtmlReport.generate_and_write
