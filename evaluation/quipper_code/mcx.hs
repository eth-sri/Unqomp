import Quipper
import Quipper.Utils.Auxiliary
import Quipper.Internal.CircLifting
import Quipper.Internal.Printing
import Quipper.Libraries.ClassicalOptim


type QbIn = (Qubit,Qubit,Qubit, Qubit,Qubit,Qubit,Qubit,Qubit,Qubit, Qubit)
type QbOut = Qubit

build_circuit
adder :: (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> Bool
adder (a0,a1, a2, a3, a4, a5, a6, a7, a8, a9) = 
  a0 && a1 && a2 && a3 && a4 && a5 && a6 && a7 && a8 && a9
    

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
