open A6.Set
open Unix

(** [time f x] measures the time it takes to run [f x] *)
let time f x =
  let start = gettimeofday () in
  let _ = f x in
  let stop = gettimeofday () in
  stop -. start

(** [insert_elements n] inserts [n] distinct elements into the set *)
let insert_elements n =
  let tree = ref empty in
  for i = 1 to n do
    tree := insert i !tree
  done

(** [median lst] calculates the median of a list of floats *)
let median lst =
  let sorted_lst = List.sort compare lst in
  let len = List.length sorted_lst in
  if len mod 2 = 1 then List.nth sorted_lst (len / 2)
  else
    let a = List.nth sorted_lst ((len / 2) - 1) in
    let b = List.nth sorted_lst (len / 2) in
    (a +. b) /. 2.0

(** [measure_multiple_samples n repetitions] performs the insertion
    [repetitions] times for [n], returning a list of times. *)
let measure_multiple_samples n repetitions =
  let rec measure_times acc count =
    if count = 0 then acc
    else
      let time_taken = time insert_elements n in
      measure_times (time_taken :: acc) (count - 1)
  in
  measure_times [] repetitions

(** [experiment] conducts the performance experiment by inserting elements for
    different values of n *)
let experiment () =
  (* We will generate n values in logarithmic scale from 100,000 to
     100,000,000 *)
  let n_values =
    [
      10000;
      50000;
      100000;
      500000;
      1000000;
      5000000;
      10000000;
      50000000;
      100000000;
      500000000;
    ]
  in

  Printf.printf "N,N log N,Time\n";

  (* Output header for CSV format *)

  (* For each value of n, run the insertion and measure the time multiple times,
     then calculate the median time *)
  List.iter
    (fun n ->
      let repetitions = 10 in
      (* You can adjust the number of repetitions here *)
      let times = measure_multiple_samples n repetitions in
      let median_time = median times in

      (* Calculate N log N using log base 10 *)
      let n_log_n = float_of_int n *. log10 (float_of_int n) in

      (* Output the results in CSV format and flush output immediately *)
      Printf.printf "%d,%g,%g\n" n n_log_n median_time;
      flush Stdlib.stdout (* Flushing the output to stdout correctly *))
    n_values

(* Run the experiment when this program is executed *)
let () = experiment ()
