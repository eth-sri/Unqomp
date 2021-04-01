import Quipper
import Quipper.Utils.Auxiliary
import Quipper.Internal.CircLifting
import Quipper.Internal.Printing
import Quipper.Libraries.ClassicalOptim

type QbIn = (Qubit,Qubit,Qubit, Qubit,Qubit,Qubit,Qubit,Qubit)
type QbOut = (Qubit,Qubit,Qubit,Qubit)

type IntSize4 = (Bool, Bool, Bool,Bool)

build_circuit
adder_FixedVal :: (Bool, Bool, Bool,Bool,Bool,Bool,Bool,Bool) -> IntSize4
adder_FixedVal (a0, a1, a2, a3,b0, b1, b2,b3) = 
  let s0 = (bool_xor a0 b0) in
  let c1 = a0 && b0 in
  let s1 = (bool_xor (bool_xor a1 b1) c1) in
  let c2 = ((a1 && b1) || (a1 && c1) || (b1 && c1)) in
  let s2 = (bool_xor (bool_xor a2 b2) c2) in
	let c3 = ((a2 && b2) || (a2 && c2) || (b2 && c2)) in
  let s3 = (bool_xor (bool_xor a3 b3) c3) in
  (s0, s1, s2,s3)

adder_circ :: QbIn
  -> Circ QbOut
adder_circ = unpack template_adder_FixedVal

adder_reversible :: (QbIn,QbOut)
  -> Circ (QbIn,QbOut)
adder_reversible = classical_to_reversible_optim adder_circ

build_circuit
concatTuples :: IntSize4 -> IntSize4 -> (Bool, Bool, Bool,Bool,Bool,Bool,Bool,Bool)
concatTuples (a0, a1, a2, a3) (b0, b1, b2, b3) = (a0, a1, a2, a3, b0, b1, b2, b3)

build_circuit
mult :: (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> (Bool,Bool,Bool,Bool)
mult (x0, x1, x2, x3, y0, y1, y2, y3) = 
	let vm1 = (False, False, False,False) in
	let a0 = (x0 && y0, x0 && y1, x0 && y2, x0 && y3) in
	let v0 = adder_FixedVal (concatTuples a0 vm1) in
	let a1 = (False, x1 && y0, x1 && y1, x1 && y2) in
	let v1 = adder_FixedVal (concatTuples a1 v0) in
  let a2 = (False, False, x2 && y0, x2 && y1) in
  let v2 = adder_FixedVal (concatTuples a2 v1) in
  let a3 = (False, False, False, x3 && y0) in
  adder_FixedVal (concatTuples a3 v2)

mult_circ :: QbIn
  -> Circ QbOut
mult_circ = unpack template_mult

mult_reversible :: (QbIn,QbOut)
  -> Circ (QbIn,QbOut)
mult_reversible = classical_to_reversible_optim mult_circ

--nb_gates :: Gatecount
--nb_gates = gatecount_of_circuit adder_reversible

main :: IO ()
main = do
  print "Adder"
  print_simple GateCount (adder_reversible)
  print "Mult"
  print_simple GateCount (mult_reversible)
  --print nb_gates
  return ()
