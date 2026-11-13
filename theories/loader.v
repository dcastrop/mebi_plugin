Set Debug "backtrace".
From MEBI Require Export Bisimilarity.
Declare ML Module "rocq-mebi.mebi".

(* The decidable premises of a rule ([Print MeBi Rules]): a Require, not
   an Import, so no names enter scope; stdlib's instances are #[global]. *)
From Stdlib Require Classes.DecidableClass.

(* The lemmas a certificate's facts apply (theories/DdTree.v,
   doc/certificate.md): required, not imported. *)
From MEBI Require DdTree.
(* the theorems from the certificate (theories/DdGame.v) *)
From MEBI Require DdGame DdEnc.
(* proofs up to reordering (theories/DdUpto.v, doc/upto.md) *)
From MEBI Require DdUpto.
MeBi Divider "Loading MeBi Plugin".
MeBi Divider.
