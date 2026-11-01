open Claudius
open Claudius_sdl

module Proj = Proj_c


let crs = Proj.Transformation.of_string ~src:"EPSG:4326" "EPSG:27700"
    (* |> Proj.Transformation.normalize_for_visualization *)


let generate_grid () =
    let h = List.init 9 (fun i ->
        let lng = -3.7 +. ((float_of_int i) /. 10.) in
        List.init 70 (fun j ->
            let lat = 53.2 +. ((float_of_int j) /. 100.) in
            (lng, lat)
        )
    ) |> List.concat
    and v =
    List.init 90 (fun i ->
        let lng = -3.8 +. ((float_of_int i) /. 100.) in
        List.init 7 (fun j ->
            let lat = 53.2 +. ((float_of_int j) /. 10.) in
            (lng, lat)
        )
    ) |> List.concat
    in
    h @ v

let wgs84_to_bng (lng, lat) =
    let src_coord = Proj.Coord.make ~x:lat ~y:lng ~z:0. ~t:0. in
    let tgt_coord = Proj.Transformation.transform crs src_coord in
    ( (Proj.Coord.x tgt_coord),  (Proj.Coord.y tgt_coord))


let boot s =
    let p = Screen.palette s in
    let fb = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) in
    let pmax = (Palette.size p) - 1 in

    let min_x, min_y = wgs84_to_bng (-3.77, 53.19)
    and max_x, max_y = wgs84_to_bng (-2.90, 53.71) in
    let width = max_x -. min_x
    and height = max_y -. min_y in

    let screen_width, screen_height = Screen.dimensions s in
    let f_screen_width = float_of_int screen_width
    and f_screen_height = float_of_int screen_height in

    let grid = generate_grid () in

    let data = Geojson.of_file "output.geojson" in
    let occs = List.filter_map (fun f ->
        let g = Feature.geometry f in
        match g with
        | Feature.Point coord -> Some (coord.longitude, coord.latitude)
        | _ -> None
    ) (Geojson.features data) in
    let occsp = List.map wgs84_to_bng occs in
    let occspp = List.map (fun (x, y) ->
        let px = int_of_float (((x -. min_x) *. f_screen_width) /. width) in
        let py = int_of_float ((y -. min_y) *. f_screen_height /. height) in
        px, (screen_height - py)
    ) occsp in
    (* let occsppp = List.map (fun (px, py) -> Primitives.Pixel ({x=px; y=py}, pmax))  occspp in *)


    let projected_grid = List.map wgs84_to_bng grid in
    let points = List.map (fun (x, y) ->
        let px = int_of_float (((x -. min_x) *. f_screen_width) /. width) in
        let py = int_of_float ((y -. min_y) *. f_screen_height /. height) in
        px, (screen_height - py)
    ) projected_grid in
    let primitives = List.map (fun (px, py) -> Primitives.Pixel ({x=px; y=py}, (pmax / 2)))  points in
    Framebuffer.render fb (primitives);
    List.iter (fun (x, y) ->
        match Framebuffer.pixel_read x y fb with
        | None -> ()
        | Some v -> (
            let nv = if v = 0 then (pmax / 2) else v + 1 in
            Framebuffer.pixel_write x y nv fb
        )
    ) occspp;
    fb

let tick _t _s fb _i =
    fb

let () =
    (Palette.of_list (List.rev (Palette.to_list (Palette.generate_mono_palette 256)))) |>
    Screen.create ~scale:1 "Day 1" (800, 480) |>
    Base.run ~boot tick
