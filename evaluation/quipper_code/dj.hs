import Quipper
import Quipper.Utils.Auxiliary
import Quipper.Internal.CircLifting
import Quipper.Internal.Printing
import Quipper.Libraries.ClassicalOptim


type QbIn = (Qubit,Qubit,Qubit, Qubit,Qubit,Qubit,Qubit,Qubit,Qubit, Qubit)
type QbOut = Qubit

build_circuit
mcx :: (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> Bool
mcx (a0,a1, a2, a3, a4, a5, a6, a7, a8, a9) = 
  a0 && a1 && a2 && a3 && a4 && a5 && a6 && a7 && a8 && a9
    

mcx_circ :: QbIn
  -> Circ QbOut
mcx_circ = unpack template_mcx

mcx_reversible :: (QbIn,QbOut)
  -> Circ (QbIn,QbOut)
mcx_reversible = classical_to_reversible_optim mcx_circ

dj :: Circ (QbIn, QbOut)
dj = do
	d0 <- qinit False
	d1 <- qinit False
	d2 <- qinit False
	d3 <- qinit False
	d4 <- qinit False
	d5 <- qinit False
	d6 <- qinit False
	d7 <- qinit False
	d8 <- qinit False
	d9 <- qinit False
	a0 <- hadamard d0
	a1 <- hadamard d1
	a2 <- hadamard d2
	a3 <- hadamard d3
	a4 <- hadamard d4
	a5 <- hadamard d5
	a6 <- hadamard d6
	a7 <- hadamard d7
	a8 <- hadamard d8
	a9 <- hadamard d9
	p0 <- qinit False
	o0 <- qnot p0
	o1 <- hadamard o0
	((b0, b1, b2, b3, b4, b5, b6, b7, b8, b9), o2) <- mcx_reversible ((a0, a1, a2, a3, a4, a5, a6, a7, a8, a9), o1)
	c0 <- hadamard b0
	c1 <- hadamard b1
	c2 <- hadamard b2
	c3 <- hadamard b3
	c4 <- hadamard b4
	c5 <- hadamard b5
	c6 <- hadamard b6
	c7 <- hadamard b7
	c8 <- hadamard b8
	c9 <- hadamard b9
	return ((c0, c1, c2, c3, c4, c5, c6, c7, c8, c9), o2)

--nb_gates :: Gatecount
--nb_gates = gatecount_of_circuit mcx_reversible

main :: IO ()
main = do
  print_simple GateCount (dj)
  --print nb_gates
  return ()
