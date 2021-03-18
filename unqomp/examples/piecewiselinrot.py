
# Adapted from https://qiskit.org/documentation/_modules/qiskit/circuit/library/arithmetic/piecewise_linear_pauli_rotations.html#PiecewiseLinearPauliRotations

"""Piecewise-linearly-controlled rotation."""
from typing import List, Optional
import numpy as np

from qiskit.circuit import QuantumRegister, QuantumCircuit
from qiskit.circuit.exceptions import CircuitError

from qiskit.circuit.library.arithmetic.linear_pauli_rotations import LinearPauliRotations

from unqomp.examples.intergercomparator import makeIntegerComparator
from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit

def _contains_zero_breakpoint(breakpoints):
    return np.isclose(0, breakpoints[0])

def makesPLR(num_state_qubits, breakpoints, slopes, offsets):
    qr_state = QuantumRegister(num_state_qubits, name='state')
    qr_target = QuantumRegister(1, name='target')
    circuit = AncillaCircuit(qr_state, qr_target)

    mapped_slopes = np.zeros_like(slopes)
    for i, slope in enumerate(slopes):
        mapped_slopes[i] = slope - sum(mapped_slopes[:i])

    mapped_offsets = np.zeros_like(offsets)
    for i, (offset, slope, point) in enumerate(zip(offsets, slopes, breakpoints)):
        mapped_offsets[i] = offset - slope * point - sum(mapped_offsets[:i])

    basis = 'Y'

    # apply comparators and controlled linear rotations
    for i, point in enumerate(breakpoints):
        if i == 0 and _contains_zero_breakpoint(breakpoints):
            # apply rotation
            lin_r = LinearPauliRotations(num_state_qubits=num_state_qubits,
                                         slope=mapped_slopes[i],
                                         offset=mapped_offsets[i], basis = 'Y')
            circuit.append(lin_r.to_gate(), qr_state[:] + [qr_target])

        else:

            comp_ancilla = circuit.new_ancilla_register(1, name = 'ac' + str(i))
            circuit.append(makeIntegerComparator(num_state_qubits, point).to_ancilla_gate(), [*qr_state[:], comp_ancilla[0]])

            # apply controlled rotation
            lin_r = LinearPauliRotations(num_state_qubits=num_state_qubits,
                                         slope=mapped_slopes[i],
                                         offset=mapped_offsets[i],
                                         basis=basis)
            circuit.append(lin_r.to_gate().control(), [comp_ancilla[0]] + qr_state[:] + [qr_target])

    return circuit