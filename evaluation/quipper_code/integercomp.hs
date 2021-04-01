import Quipper
import Quipper.Utils.Auxiliary
import Quipper.Internal.CircLifting
import Quipper.Internal.Printing
import Quipper.Libraries.ClassicalOptim

-- n=4
-- val = 3

type QbIn = (Qubit,Qubit,Qubit,Qubit)
type QbOut = Qubit
type IntSize4 = (Bool, Bool, Bool, Bool)


build_circuit
adder :: (Bool,Bool,Bool,Bool) -> Bool
adder (v0, v1, v2,v3) = 
  let (t0, t1, t2, t3) = (True, True, False, True) in -- compl2 of val to compare to
  let a0 = t0 && v0 in
  let a1 = if t1 then v1 || a0 else v1 && a0 in
  let a2 = if t2 then v2 || a1 else v2 && a1 in
  let res = if t3 then v3 || a2 else v3 && a2 in
  res
    

adder_circ :: QbIn
  -> Circ QbOut
adder_circ = unpack template_adder

adder_reversible :: (QbIn,QbOut)
  -> Circ (QbIn,QbOut)
adder_reversible = classical_to_reversible_optim adder_circ

--nb_gates :: Gatecount
--nb_gates = gatecount_of_circuit adder_reversible

main :: IO ()
main = do
  print_simple GateCount (adder_reversible)
  --print nb_gates
  return ()
