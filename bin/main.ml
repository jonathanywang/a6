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

(** [experiment] conducts the performance experiment by inserting elements for
    different values of n *)
let experiment () =
  (* We will generate n values in logarithmic scale from 100,000 to
     100,000,000 *)
  let n_values =
    [
      500000;
      750000;
      1000000;
      5000000;
      7500000;
      10000000;
      50000000;
      75000000;
      100000000;
      500000000;
    ]
  in

  Printf.printf "N, Time, N Log N\n";

  (* For each value of n, run the insertion and measure the time once *)
  List.iter
    (fun n ->
      (* Measure the time for a single insertion run *)
      let time_taken = time insert_elements n in

      (* Calculate N log N using log base 10 *)
      let n_log_n = float_of_int n *. log10 (float_of_int n) in

      (* Output the results in CSV format and flush output immediately *)
      Printf.printf "%d,%g,%g\n%!" n time_taken n_log_n;
      flush Stdlib.stdout (* Flushing the output to stdout correctly *))
    n_values

(* Run the experiment when this program is executed *)
let () = experiment ()
