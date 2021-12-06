Require Import  Coq.Lists.List.
Require Import Coq.NArith.BinNat.

Open Scope N_scope.
Open Scope list_scope.

Import ListNotations.

Inductive Datum.

Definition example_data : list Datum := [].
