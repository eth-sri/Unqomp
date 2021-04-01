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
mcx_reversible = classical_to_reversible mcx_circ

build_circuit
mcx_m :: (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> Bool
mcx_m (a0,a1, a2, a3, a4, a5, a6, a7, a8) = 
  a0 && a1 && a2 && a3 && a4 && a5 && a6 && a7 && a8
    

mcx_circ_m :: (Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit,Qubit)
  -> Circ QbOut
mcx_circ_m = unpack template_mcx_m

mcx_reversible_m :: ((Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit,Qubit),QbOut)
  -> Circ ((Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit, Qubit,Qubit),QbOut)
mcx_reversible_m = classical_to_reversible_optim mcx_circ_m

h_reg :: QbIn -> Circ (QbIn)
h_reg (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) = do
	b0 <- hadamard a0
	b1 <- hadamard a1
	b2 <- hadamard a2
	b3 <- hadamard a3
	b4 <- hadamard a4
	b5 <- hadamard a5
	b6 <- hadamard a6
	b7 <- hadamard a7
	b8 <- hadamard a8
	b9 <- hadamard a9
	return (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9)

h_reg_m :: QbIn -> Circ (QbIn)
h_reg_m (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) = do
	b0 <- hadamard a0
	b1 <- hadamard a1
	b2 <- hadamard a2
	b3 <- hadamard a3
	b4 <- hadamard a4
	b5 <- hadamard a5
	b6 <- hadamard a6
	b7 <- hadamard a7
	b8 <- hadamard a8
	return (b0, b1, b2, b3, b4, b5, b6, b7, b8, a9)

x_reg :: QbIn -> Circ (QbIn)
x_reg (a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) = do
	b0 <- qnot a0
	b1 <- qnot a1
	b2 <- qnot a2
	b3 <- qnot a3
	b4 <- qnot a4
	b5 <- qnot a5
	b6 <- qnot a6
	b7 <- qnot a7
	b8 <- qnot a8
	b9 <- qnot a9
	return (b0, b1, b2, b3, b4, b5, b6, b7, b8, b9)

amplitude_ampl :: (QbIn, QbOut) -> Circ (QbIn, QbOut)
amplitude_ampl (a, o0) = do
	(b, o1) <- mcx_reversible (a, o0)
	c <- h_reg b
	d <- x_reg c
	(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9) <- h_reg_m d
	((e0, e1, e2, e3, e4, e5, e6, e7, e8), e9) <- mcx_reversible_m ((d0, d1, d2, d3, d4, d5, d6, d7, d8), d9)
	ep <- h_reg_m (e0, e1, e2, e3, e4, e5, e6, e7, e8, e9)
	f <- x_reg ep
	g <- h_reg f
	return (g, o1)

repeatAmpl :: Int -> (QbIn, QbOut) -> Circ (QbIn, QbOut)
repeatAmpl 0 (a, b) = do
	return (a, b)
repeatAmpl n (a, b) = do
	(c, d) <- amplitude_ampl (a, b)
	(e, f) <- repeatAmpl (n-1) (c, d)
	return (e, f)

grover :: Circ (QbIn, QbOut)
grover = do
	o0 <- qinit False
	o1 <- qnot o0
	o2 <- hadamard o1
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
	a <- h_reg (d0, d1, d2, d3, d4, d5, d6, d7, d8, d9)
	(b, o3) <- repeatAmpl 25 (a, o2)
	return (b, o3)
	

	
	
--nb_gates :: Gatecount
--nb_gates = gatecount_of_circuit mcx_reversible

main :: IO ()
main = do
  print_simple GateCount (grover)
  --print nb_gates
  return ()
