# Adapted from https://www.qiskit.org/documentation/_modules/qiskit/circuit/library/arithmetic/integer_comparator.html#IntegerComparator
import numpy as np

from qiskit.circuit import QuantumRegister, QuantumCircuit
from qiskit.circuit.exceptions import CircuitError
from qiskit.circuit.library.boolean_logic import OR

from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit

      
def _get_twos_complement(n, val):
    """Returns the 2's complement of ``self.value`` as array.

    Returns:
         The 2's complement of ``self.value``.
    """
    twos_complement = pow(2, n) - int(np.ceil(val))
    twos_complement = '{0:b}'.format(twos_complement).rjust(n, '0')
    twos_complement = \
        [1 if twos_complement[i] == '1' else 0 for i in reversed(range(len(twos_complement)))]
    return twos_complement

def makeIntegerComparator(num_state_qubits, value, geq  = True, with_uncomp = True):
    """Build the comparator circuit."""
    or_gate = OR(2).to_gate()
    qr_state = QuantumRegister(num_state_qubits, name='state')
    q_compare = QuantumRegister(1, name='compare')
    circuit = AncillaCircuit(qr_state, q_compare)
    circuit.addQfreeGate(or_gate)
    qr_ancilla = circuit.new_ancilla_register(num_state_qubits - 1, name = 'ancilla') if num_state_qubits > 1 else None

    if value <= 0:  # condition always satisfied for non-positive values
        if geq:  # otherwise the condition is never satisfied
            circuit.x(q_compare)
    # condition never satisfied for values larger than or equal to 2^n
    elif value < pow(2, num_state_qubits):

        if num_state_qubits > 1:
            twos = _get_twos_complement(num_state_qubits, value)
            for i in range(num_state_qubits):
                if i == 0:
                    if twos[i] == 1:
                        circuit.cx(qr_state[i], qr_ancilla[i])
                elif i < num_state_qubits - 1:
                    if twos[i] == 1:
                        circuit.append(or_gate, [qr_state[i], qr_ancilla[i - 1], qr_ancilla[i]])
                    else:
                        circuit.ccx(qr_state[i], qr_ancilla[i - 1], qr_ancilla[i])
                else:
                    if twos[i] == 1:
                        circuit.append(or_gate, [qr_state[i], qr_ancilla[i - 1], q_compare])
                    else:
                        circuit.ccx(qr_state[i], qr_ancilla[i - 1], q_compare)

            # flip result bit if geq flag is false
            if not geq:
                circuit.x(q_compare)

        else:

            # num_state_qubits == 1 and value == 1:
            circuit.cx(qr_state[0], q_compare)

            # flip result bit if geq flag is false
            if not geq:
                circuit.x(q_compare)

    else:
        if not geq:  # otherwise the condition is never satisfied
            circuit.x(q_compare)

    return circuit