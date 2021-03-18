# Adapted from https://qiskit.org/documentation/_modules/qiskit/circuit/library/arithmetic/weighted_adder.html#WeightedAdder
from qiskit.circuit import QuantumRegister, QuantumCircuit
import numpy as np

from unqomp.uncomputation import uncomputeAllAncillas
from unqomp.ancillaallocation import AncillaRegister, AncillaCircuit

def makeWeightedAdder(num_state_qubits, weights):
    # Straightforward implementation using Unqomp, uses less gate but more qubits
    num_sum_qubits = int(np.floor(np.log2(sum(weights))) + 1) if sum(weights) > 0 else 1 
    # The number of sum qubits in the circuit
    num_carry_qubits = num_sum_qubits -1
    # The number of carry qubits required to compute the sum.

    for i, weight in enumerate(weights):
        if not np.isclose(weight, np.round(weight)):
            raise ValueError('Non-integer weights are not supported!')
        weights[i] = np.round(weight)

    num_result_qubits = num_state_qubits + num_sum_qubits

    qr_state = QuantumRegister(num_state_qubits, name = 'state')
    qr_sum = QuantumRegister(num_sum_qubits, name = 'sum')
    circuit = AncillaCircuit(qr_state, qr_sum)
    #print(num_state_qubits)
    #print(num_sum_qubits)

    # loop over state qubits and corresponding weights
    for i, weight in enumerate(weights):
        # only act if non-trivial weight
        if np.isclose(weight, 0):
            continue

        # get state control qubit
        q_state = qr_state[i]

        # get bit representation of current weight
        weight_binary = '{0:b}'.format(int(weight)).rjust(num_sum_qubits, '0')[::-1]
        #print("carry qb" + str(num_carry_qubits))
        qr_carry = circuit.new_ancilla_register(num_carry_qubits, name = "anccarryite" + str(i))

        # loop over bits of current weight and add them to sum and carry registers
        for j, bit in enumerate(weight_binary):
            if bit == '1':
                if num_sum_qubits == 1:
                    circuit.cx(q_state, qr_sum[j])
                elif j == 0:
                    # compute (q_sum[0] + 1) into (q_sum[0], q_carry[0])
                    # - controlled by q_state[i]
                    circuit.ccx(q_state, qr_sum[j], qr_carry[j])
                    circuit.cx(q_state, qr_sum[j])
                elif j == num_sum_qubits - 1:
                    # compute (q_sum[j] + q_carry[j-1] + 1) into (q_sum[j])
                    # - controlled by q_state[i] / last qubit,
                    # no carry needed by construction
                    circuit.cx(q_state, qr_sum[j])
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                else:
                    # compute (q_sum[j] + q_carry[j-1] + 1) into (q_sum[j], q_carry[j])
                    # - controlled by q_state[i]
                    circuit.mcx([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], negated_ctrls = [1, 2])
                    circuit.cx(q_state, qr_carry[j])
                    circuit.cx(q_state, qr_sum[j])
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
            else:
                if num_sum_qubits == 1:
                    pass  # nothing to do, since nothing to add
                elif j == 0:
                    pass  # nothing to do, since nothing to add
                elif j == num_sum_qubits-1:
                    # compute (q_sum[j] + q_carry[j-1]) into (q_sum[j])
                    # - controlled by q_state[i] / last qubit,
                    # no carry needed by construction
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                else:
                    # compute (q_sum[j] + q_carry[j-1]) into (q_sum[j], q_carry[j])
                    # - controlled by q_state[i]
                    circuit.mcx([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j])
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
    return circuit

def makesQiskitWA(num_state_qubits, weights):
    # Qiskit implementation still has a regression bug: weighted adder uses the default 'noancilla' mode instead of basic when asked for it
    num_sum_qubits = int(np.floor(np.log2(sum(weights))) + 1) if sum(weights) > 0 else 1 
    num_result_qubits = num_state_qubits + num_sum_qubits
    num_carry_qubits = num_sum_qubits - 1
    num_control_qubits = num_sum_qubits > 2

    qr_state = QuantumRegister(num_state_qubits, name = 'state')
    qr_sum = QuantumRegister(num_sum_qubits, name = 'sum')
    qr_carry = QuantumRegister(num_carry_qubits, name = 'carry')
    qr_control = QuantumRegister(1, name='ctrl') if num_control_qubits > 0 else []
    circ = QuantumCircuit(qr_state, qr_sum, qr_carry)
    if num_control_qubits > 0:
        circ.add_register(qr_control)

    # loop over state qubits and corresponding weights
    for i, weight in enumerate(weights):
        # only act if non-trivial weight
        if np.isclose(weight, 0):
            continue

        # get state control qubit
        q_state = qr_state[i]

        # get bit representation of current weight
        weight_binary = '{0:b}'.format(int(weight)).rjust(num_sum_qubits, '0')[::-1]

        # loop over bits of current weight and add them to sum and carry registers
        for j, bit in enumerate(weight_binary):
            if bit == '1':
                if num_sum_qubits == 1:
                    circ.cx(q_state, qr_sum[j])
                elif j == 0:
                    # compute (q_sum[0] + 1) into (q_sum[0], q_carry[0])
                    # - controlled by q_state[i]
                    circ.ccx(q_state, qr_sum[j], qr_carry[j])
                    circ.cx(q_state, qr_sum[j])
                elif j == num_sum_qubits - 1:
                    # compute (q_sum[j] + q_carry[j-1] + 1) into (q_sum[j])
                    # - controlled by q_state[i] / last qubit,
                    # no carry needed by construction
                    circ.cx(q_state, qr_sum[j])
                    circ.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                else:
                    # compute (q_sum[j] + q_carry[j-1] + 1) into (q_sum[j], q_carry[j])
                    # - controlled by q_state[i]
                    circ.x(qr_sum[j])
                    circ.x(qr_carry[j - 1])
                    circ.mct([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], qr_control, mode='basic')
                    circ.cx(q_state, qr_carry[j])
                    circ.x(qr_sum[j])
                    circ.x(qr_carry[j - 1])
                    circ.cx(q_state, qr_sum[j])
                    circ.ccx(q_state, qr_carry[j - 1], qr_sum[j])
            else:
                if num_sum_qubits == 1:
                    pass  # nothing to do, since nothing to add
                elif j == 0:
                    pass  # nothing to do, since nothing to add
                elif j == num_sum_qubits-1:
                    # compute (q_sum[j] + q_carry[j-1]) into (q_sum[j])
                    # - controlled by q_state[i] / last qubit,
                    # no carry needed by construction
                    circ.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                else:
                    # compute (q_sum[j] + q_carry[j-1]) into (q_sum[j], q_carry[j])
                    # - controlled by q_state[i]
                    circ.mct([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], qr_control, mode='basic')
                    circ.ccx(q_state, qr_carry[j - 1], qr_sum[j])

        # uncompute carry qubits
        for j in reversed(range(len(weight_binary))):
            bit = weight_binary[j]
            if bit == '1':
                if num_sum_qubits == 1:
                    pass
                elif j == 0:
                    circ.x(qr_sum[j])
                    circ.ccx(q_state, qr_sum[j], qr_carry[j])
                    circ.x(qr_sum[j])
                elif j == num_sum_qubits - 1:
                    pass
                else:
                    circ.x(qr_carry[j - 1])
                    circ.mct([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], qr_control, mode='basic')
                    circ.cx(q_state, qr_carry[j])
                    circ.x(qr_carry[j - 1])
            else:
                if num_sum_qubits == 1:
                    pass
                elif j == 0:
                    pass
                elif j == num_sum_qubits - 1:
                    pass
                else:
                    # compute (q_sum[j] + q_carry[j-1]) into (q_sum[j], q_carry[j])
                    # - controlled by q_state[i]
                    circ.x(qr_sum[j])
                    circ.mct([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], qr_control, mode='basic')
                    circ.x(qr_sum[j])

    return circ

def makeWeightedAdderWOExtraCtrlsQb(num_state_qubits, weights):
    # Implementation using Unqomp but enforcing local uncomputation, uses less qubits, but is not fully modular, ancillas have to be taken care of manually
    def neg_mct_gate(negated_ctrls):
        #circuit.mcx([q_state, qr_sum[j], qr_carry[j - 1]], qr_carry[j], negated_ctrls = [1, 2])
        ctrls = QuantumRegister(3)
        anc = QuantumRegister(1)
        targ = QuantumRegister(1)
        neg_mct = QuantumCircuit(ctrls, anc, targ) #[ctrl0...ctrl3, anc, targ]
        for i in negated_ctrls:
            neg_mct.x(ctrls[i])
        neg_mct.mcx(ctrls, targ, anc, mode='basic')
        for i in negated_ctrls:
            neg_mct.x(ctrls[i])
        return neg_mct.to_gate()

    num_sum_qubits = int(np.floor(np.log2(sum(weights))) + 1) if sum(weights) > 0 else 1 
    # The number of sum qubits in the circuit
    num_carry_qubits = num_sum_qubits -1
    # The number of carry qubits required to compute the sum.

    for i, weight in enumerate(weights):
        if not np.isclose(weight, np.round(weight)):
            raise ValueError('Non-integer weights are not supported!')
        weights[i] = np.round(weight)

    num_result_qubits = num_state_qubits + num_sum_qubits

    qr_state = QuantumRegister(num_state_qubits, name = 'state')
    qr_sum = QuantumRegister(num_sum_qubits, name = 'sum')
    qr_ctrl = QuantumRegister(1, name='ctrl')
    circuit = AncillaCircuit(qr_state, qr_sum, qr_ctrl)

    neg_mct_g = neg_mct_gate([1, 2])
    mct_g = neg_mct_gate([])

    circuit.addQfreeGate(neg_mct_g)
    circuit.addQfreeGate(mct_g)

    # loop over state qubits and corresponding weights
    for i, weight in enumerate(weights):
        # only act if non-trivial weight
        if np.isclose(weight, 0):
            continue

        # get state control qubit
        q_state = qr_state[i]

        # get bit representation of current weight
        weight_binary = '{0:b}'.format(int(weight)).rjust(num_sum_qubits, '0')[::-1]
        #print("carry qb" + str(num_carry_qubits))
        qr_carry = circuit.new_ancilla_register(num_carry_qubits, name = "anccarryite" + str(i))

        # loop over bits of current weight and add them to sum and carry registers
        for j, bit in enumerate(weight_binary):
            if bit == '1':
                if num_sum_qubits == 1:
                    circuit.cx(q_state, qr_sum[j])
                elif j == 0:
                    # compute (q_sum[0] + 1) into (q_sum[0], q_carry[0])
                    # - controlled by q_state[i]
                    circuit.ccx(q_state, qr_sum[j], qr_carry[j])
                    circuit.cx(q_state, qr_sum[j])
                elif j == num_sum_qubits - 1:
                    # compute (q_sum[j] + q_carry[j-1] + 1) into (q_sum[j])
                    # - controlled by q_state[i] / last qubit,
                    # no carry needed by construction
                    circuit.cx(q_state, qr_sum[j])
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                else:
                    # compute (q_sum[j] + q_carry[j-1] + 1) into (q_sum[j], q_carry[j])
                    # - controlled by q_state[i]
                    circuit.append(neg_mct_g, [q_state, qr_sum[j], qr_carry[j-1], qr_ctrl, qr_carry[j]])
                    circuit.cx(q_state, qr_carry[j])
                    circuit.cx(q_state, qr_sum[j])
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
            else:
                if num_sum_qubits == 1:
                    pass  # nothing to do, since nothing to add
                elif j == 0:
                    pass  # nothing to do, since nothing to add
                elif j == num_sum_qubits-1:
                    # compute (q_sum[j] + q_carry[j-1]) into (q_sum[j])
                    # - controlled by q_state[i] / last qubit,
                    # no carry needed by construction
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
                else:
                    # compute (q_sum[j] + q_carry[j-1]) into (q_sum[j], q_carry[j])
                    # - controlled by q_state[i]
                    circuit.append(mct_g, [q_state, qr_sum[j], qr_carry[j-1], qr_ctrl, qr_carry[j]])
                    circuit.ccx(q_state, qr_carry[j - 1], qr_sum[j])
    return circuit
