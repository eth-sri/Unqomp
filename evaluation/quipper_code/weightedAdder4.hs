import Quipper
import Quipper.Utils.Auxiliary
import Quipper.Internal.CircLifting
import Quipper.Internal.Printing
import Quipper.Libraries.ClassicalOptim

-- n=4
-- vals = [1, 2, 3, 2]

type QbIn = (Qubit,Qubit,Qubit,Qubit)
type QbOut = (Qubit,Qubit,Qubit,Qubit,Qubit,Qubit)
type IntSize4 = (Bool, Bool, Bool, Bool)

zero = (False, False, False)
one = (True, False, False)
two = (False, True, False)
three = (True, True, False)

build_circuit
adder_FixedVal :: Bool -> (Bool, Bool, Bool, Bool, Bool,Bool) -> IntSize4 -> (Bool, Bool, Bool, Bool, Bool,Bool)
adder_FixedVal ctrl (a0, a1, a2, a3, a4, a5) (b0, b1, b2, b3) = (s0, s1, s2, s3, s4,s5)
	where 
  	s0 = ctrl && (bool_xor a0 b0)
  	c1 = ctrl && a0 && b0
  	s1 = ctrl && (bool_xor (bool_xor a1 b1) c1)
  	c2 = ctrl && ((a1 && b1) || (a1 && c1) || (b1 && c1))
  	s2 = ctrl && (bool_xor (bool_xor a2 b2) c2)
  	c3 = ctrl && ((a2 && b2) || (a2 && c2) || (b2 && c2))
  	s3 = ctrl && (bool_xor (bool_xor a3 b3) c3)
  	c4 = ctrl && (a3 && c3)
  	s4 = ctrl && (bool_xor a4 c4)
  	c5 = ctrl && (a4 && c4)
  	s5 = ctrl && (bool_xor a5 c5)

build_circuit
adder :: (Bool,Bool,Bool,Bool) -> (Bool,Bool,Bool,Bool,Bool,Bool)
adder (v0, v1, v2,v3) = r3
	where 
  	r0 = (s0, s1, s2, s3, s4,s5) 
			where 
				s0 = v0 && True
      	s1 = v0 && True
      	s2 = v0 && True
      	s3 = v0 && True
      	s4 = v0 && False
      	s5 = v0 && False
  	r1 = adder_FixedVal v1 r0 (True, True, True,True)
  	r2 = adder_FixedVal v2 r1 (True, True, True,True)
  	r3 = adder_FixedVal v3 r2 (True, True, True,True)
    

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
