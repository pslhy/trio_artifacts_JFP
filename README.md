# Artifacts for "Inductive Synthesis of Recursive Functional Programs with Trace-based Pruning"

## Build
```
$ git clone https://github.com/pslhy/trio_artifacts_2024.git
$ cd trio_artifacts_2024
$ ./build
$ . setenv # or ($ bash setenv)
$ make
```

## Running All the Experiments
```
$ python3 artifact.py  [ io | ref | ablation ] [--timeout <sec> (default: 120)]
```
To run all the IO Example Benchmarks, run the following command:
```
$ python3 artifact.py io
```
To run all the Reference Implementation Benchmarks, run the following command:
```
$ python3 artifact.py ref
```

The following command will run all the Ablations Study section:
```
$ python3 artifact.py ablation
```

## Effectiveness of Trio with IO Specifications (Table 2 in the paper)
After running the above commands you can see the results of Table 2 in the paper by running the following command:
```
$ python3 artifact.py --print_result 1
```
## Effectiveness of Trio with Ref Specifications (Table 3 in the paper)
You can reproduce the results of Table 3 in the paper by running the following command:
```
$ python3 artifact.py --print_result 2
```
## Effectiveness of the block-based pruning and library sampling techniques
To run following command, you can see the summary of ablation study:
```
$ python3 artifact.py --print_result 3
```

## Sensitivity to the Quntity and Quality of Examples
To run following command, you can see the summary of sensitivity to the quantity and quality of examples:
```
$ python3 artifact.py --print_result 4
```

## Running Trio for a single benchmark
You can run Trio to solve single benchmark as follows:
```
$ burst/BurstCmdLine.exe -use-trio [benchmark file]
```
and if you run with option of Trio,

```
$ burst/BurstCmdLine.exe -use-trio -trio-options “<trio options>” [benchmark file]
```

For example, to solve the single benchmark in benchmarks/io/nat_mul.mls
```
$ burst/BurstCmdLine.exe -print-data -use-trio benchmarks/io/nat_mul.mls
```
will print the following output:
```
fix (f : nat * nat -> nat) =
  fun (x:nat * nat) ->
    match x . 1 with
      | O _ -> x . 1
      | S _ -> add (f (Un_S (x . 1), x . 0)) (x . 0)
Size: 27
Iter: 0
```
At 1-5 lines show solution, and other lines shows solution size and num of iter times by -print-data option.

<!-- Not complete to run -->
## SyRup - Build and Run
```
$ opam install --yes dune qcheck z3 ppx_inline_test core core_unix iter bark menhir.20211128 ppx_deriving smart-print async_unix js_of_ocaml-ppx
```
```
$ cd syrup
$ dune build
```
```
$ syrup/syrup syrup bool_band "(T (), T ()) -> T (); (T (), F ()) -> F (); (F (), T ()) -> F ();"
```

## Acknowledgement 
This artifact is based on the artifact for [Burst](https://github.com/amiltner/BurstArtifactEvaluation)
