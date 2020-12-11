open Graphics;;

(* Q. 1.2.1 *)
let left, right, down, up = 0., 300., 0., 500.;;

open_graph (" " ^ (string_of_int (int_of_float (right -. left))) ^ "x" ^ string_of_int (int_of_float (up -. down)));;

Graphics.auto_synchronize false;;

(* Q. 1.2.2 *)
let ball, paddle, thick = 5, 50, 5;;

let draw_ball x y = fill_circle x y ball;;

(* Q. 1.2.3 *)
let new_position pos vit = pos +. vit;;

(* Q. 1.2.4 *)
let draw_paddle x = fill_rect x (int_of_float down) paddle thick;;

let position_paddle () =
    let (mx, _) = mouse_pos () in
        mx - (paddle / 2);;

(* Q. 1.2.5 *)
let bounce_x x vx =
    if (x -. ((float_of_int ball) /. 2.)) < left || (x +. ((float_of_int ball) /. 2.)) > right
        then vx *. (-1.)
    else vx;;

let bounce_y x y vy p =
    if (y +. ((float_of_int ball) /. 2.)) > up
        then vy *. (-1.)
    else if y < down +. (float_of_int thick) && x > p && x < p +. (float_of_int paddle)
        then vy *. (-1.)
    else vy;;

(* Q. 1.2.6 *)
let rec game x y vx vy =
    clear_graph ();
    draw_paddle (position_paddle ());
    draw_ball (int_of_float x) (int_of_float y);
    synchronize ();
    let vx = bounce_x x vx in
        let vy = bounce_y x y vy (float_of_int (position_paddle ())) in
            let x = new_position x vx in
                let y = new_position y vy in
                    game x y vx vy;;

game ((right -. left) /. 2.) ((up -. down) /. 2.) 0.02 0.02;;