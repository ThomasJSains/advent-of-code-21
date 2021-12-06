Require Import Coq.Lists.List.
Require Import Coq.NArith.BinNat.

Open Scope list_scope.
Open Scope N_scope.

Import ListNotations.

Definition test_input := [ 3 ; 4 ; 3 ; 1 ; 2 ].

Definition input := [ 4 ; 3 ; 3 ; 5 ; 4 ; 1 ; 2 ; 1 ; 3 ; 1 ; 1 ; 1 ; 1 ; 1 ; 2 ; 4 ; 1 ; 3 ; 3 ; 1 ; 1 ; 1 ; 1 ; 2 ; 3 ; 1 ; 1 ; 1 ; 4 ; 1 ; 1 ; 2 ; 1 ; 2 ; 2 ; 1 ; 1 ; 1 ; 1 ; 1 ; 5 ; 1 ; 1 ; 2 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 3 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 5 ; 1 ; 4 ; 2 ; 1 ; 1 ; 2 ; 1 ; 3 ; 1 ; 1 ; 2 ; 2 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 4 ; 1 ; 3 ; 2 ; 2 ; 3 ; 1 ; 1 ; 1 ; 4 ; 1 ; 1 ; 1 ; 1 ; 5 ; 1 ; 1 ; 1 ; 5 ; 1 ; 1 ; 3 ; 1 ; 1 ; 2 ; 4 ; 1 ; 1 ; 3 ; 2 ; 4 ; 1 ; 1 ; 1 ; 1 ; 1 ; 5 ; 5 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 4 ; 1 ; 1 ; 1 ; 3 ; 2 ; 1 ; 1 ; 5 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 5 ; 4 ; 1 ; 5 ; 1 ; 3 ; 4 ; 1 ; 1 ; 1 ; 1 ; 2 ; 1 ; 2 ; 1 ; 1 ; 1 ; 2 ; 2 ; 1 ; 2 ; 3 ; 5 ; 1 ; 1 ; 1 ; 1 ; 3 ; 5 ; 1 ; 1 ; 1 ; 2 ; 1 ; 1 ; 4 ; 1 ; 1 ; 5 ; 1 ; 4 ; 1 ; 2 ; 1 ; 3 ; 1 ; 5 ; 1 ; 4 ; 3 ; 1 ; 3 ; 2 ; 1 ; 1 ; 1 ; 2 ; 2 ; 1 ; 1 ; 1 ; 1 ; 4 ; 5 ; 1 ; 1 ; 1 ; 1 ; 1 ; 3 ; 1 ; 3 ; 4 ; 1 ; 1 ; 4 ; 1 ; 1 ; 3 ; 1 ; 3 ; 1 ; 1 ; 4 ; 5 ; 4 ; 3 ; 2 ; 5 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 2 ; 1 ; 5 ; 2 ; 5 ; 3 ; 1 ; 1 ; 1 ; 1 ; 1 ; 3 ; 1 ; 1 ; 1 ; 1 ; 5 ; 1 ; 2 ; 1 ; 2 ; 1 ; 1 ; 1 ; 1 ; 2 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 1 ; 3 ; 3 ; 1 ; 1 ; 5 ; 1 ; 3 ; 5 ; 5 ; 1 ; 1 ; 1 ; 2 ; 1 ; 2 ; 1 ; 5 ; 1 ; 1 ; 1 ; 1 ; 2 ; 1 ; 1 ; 1 ; 2 ; 1 ].

Inductive LanternFish := 
    | Populations (zero one two three four five six seven eight : N)
    .

Definition default_pops := Populations 0 0 0 0 0 0 0 0 0.

Fixpoint processData (input : list N) (dataSoFar : LanternFish) := 
    match input with 
    | x :: xs => processData xs (match dataSoFar with
        | Populations a b c d e f g h i => match x with 
            | 0 => Populations (a+1) b c d e f g h i
            | 1 => Populations a (b+1) c d e f g h i
            | 2 => Populations a b (c+1) d e f g h i
            | 3 => Populations a b c (d+1) e f g h i
            | 4 => Populations a b c d (e+1) f g h i
            | 5 => Populations a b c d e (f+1) g h i
            | 6 => Populations a b c d e f (g+1) h i
            | 7 => Populations a b c d e f g (h+1) i
            | 8 => Populations a b c d e f g h (i+1)
            | _ => dataSoFar
            end
        end
    )
    | [] => dataSoFar
    end.

Definition data := processData input default_pops.

Definition population (data : LanternFish) :=
    match data with 
    | Populations a b c d e f g h i => a+b+c+d+e+f+g+h+i
    end. 

Fixpoint step (n : nat) (data : LanternFish) := 
    match n with
    | 0%nat => population data
    | S n' => step n' (match data with
        | Populations a b c d e f g h i => Populations b c d e f g (h + a) i a
        end)
    end.

Definition part1 := step (80%nat) (processData input default_pops).
Definition part2 := step (256%nat) (processData input default_pops).

Compute part1.
Compute part2.