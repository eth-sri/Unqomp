# taken from https://qiskit.org/documentation/_modules/qiskit/circuit/library/standard_gates/x.html#XGate
from qiskit.circuit import QuantumRegister, QuantumCircuit
from qiskit.circuit.library.standard_gates.x import RCCXGate, CCXGate
from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit

def makeMCX(n):
    q_controls = QuantumRegister(n, name='ctrls')
    q_target = QuantumRegister(1, name='target')
    circuit = AncillaCircuit(q_controls, q_target)

    if n <=2:
        if n==1:
            circuit.cx(q_controls[0], q_target)
        else:
            circuit.ccx(q_controls[0], q_controls[1], q_target)
        return circuit

    q_ancillas = circuit.new_ancilla_register(n-2, name = 'anc')

    circuit.ccx(q_controls[0], q_controls[1], q_ancillas[0])
    i = 0
    for j in range(2, n - 1):
        circuit.ccx(q_controls[j], q_ancillas[i], q_ancillas[i + 1])
        i += 1

    circuit.ccx(q_controls[-1], q_ancillas[i], q_target)

    return circuit

def makeMCRY(theta, n):
    q_controls = QuantumRegister(n, name='ctrls')
    q_target = QuantumRegister(1, name='target')
    q_ancilla = AncillaRegister(1, name='anc')

    circuit = AncillaCircuit(q_controls, q_target, q_ancilla)
    circuit.mcx(q_controls[:], q_ancilla)
    circuit.cry(theta, q_ancilla, q_target)
    
    return circuit

def makeQiskitMCRY(theta, n):
    # Qiskit implementation still has a regression bug: mcry uses the default 'noancilla' mode instead of basic when asked for it
    q_controls = QuantumRegister(n, name='ctrls')
    q_target = QuantumRegister(1, name='target')
    q_ancillas = QuantumRegister(n - 2, name='anc') if n> 2 else None

    circuit = QuantumCircuit(q_controls, q_target)
    if(n>2):
        circuit.add_register(q_ancillas)
    circuit.u3(theta / 2, 0, 0, q_target)
    circuit.mct(q_controls, q_target, q_ancillas, mode='basic')
    circuit.u3(-theta / 2, 0, 0, q_target)
    circuit.mct(q_controls, q_target, q_ancillas, mode='basic') 

    return circuit

