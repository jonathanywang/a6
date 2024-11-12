open A6.Set
open Unix

(** [time f x] is the time, in seconds, it takes to run function [f] with
    argument [x].
    @param f the function to time
    @param x the argument to apply to [f]
    @return the time in seconds taken to execute [f x] *)
let time f x =
  let start = gettimeofday () in
  let _ = f x in
  let stop = gettimeofday () in
  stop -. start

(** [insert_elements n] inserts [n] distinct elements (1 to [n]) into a set,
    starting with an empty set.
    @param n the number of elements to insert into the set
    @return unit *)
let insert_elements n =
  let tree = ref empty in
  for i = 1 to n do
    tree := insert i !tree
  done

(** [experiment] runs a performance test for insertion operations by measuring
    the time taken to insert a range of elements into the set. The experiment is
    conducted for various values of [n] in logarithmic increments, where [n]
    represents the number of elements inserted. For each [n], it prints:
    - The value of [n]
    - The time taken to insert [n] elements
    - The value of [n * log(n)] as a theoretical comparison

    @return unit *)
let experiment () =
  (* List of values of n to test, ranging from 500,000 to 500,000,000 *)
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

(* Runs the experiment when the program is executed *)
let () = experiment ()
