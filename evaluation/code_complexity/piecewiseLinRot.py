#without unqomp, from https://github.com/Qiskit/qiskit-terra/blob/d7e469e33c212e8acf39308497bd312cfe792ca6/qiskit/circuit/library/arithmetic/piecewise_linear_pauli_rotations.py
def PLR():
        mapped_slopes = np.zeros_like(self.slopes)
        for i, slope in enumerate(self.slopes):
            mapped_slopes[i] = slope - sum(mapped_slopes[:i])
        mapped_offsets = np.zeros_like(self.offsets)
        for i, (offset, slope, point) in enumerate(zip(self.offsets, self.slopes, self.breakpoints)):
            mapped_offsets[i] = offset - slope * point - sum(mapped_offsets[:i])
 def contains_zero_breakpoint(self) -> bool:
        return np.isclose(0, self.breakpoints[0])
				qr_state = QuantumRegister(num_state_qubits)
        qr_target = QuantumRegister(1)
				if len(self.breakpoints) > 1:
                num_ancillas = num_state_qubits - 1 + len(self.breakpoints)
                if self.contains_zero_breakpoint:
                    num_ancillas -= 1
                qr_ancilla = AncillaRegister(num_ancillas)
                self.add_register(qr_ancilla)
       for i, point in enumerate(self.breakpoints):
            if i == 0 and self.contains_zero_breakpoint:
                lin_r = LinearPauliRotations(num_state_qubits=self.num_state_qubits,
                                             slope=self.mapped_slopes[i],
                                             offset=self.mapped_offsets[i],
                                             basis=self.basis)
                self.append(lin_r.to_gate(), qr_state[:] + qr_target)
            else:
                if self.contains_zero_breakpoint:
                    i_compare = i - 1
                else:
                    i_compare = i
                comp = IntegerComparator(num_state_qubits=self.num_state_qubits, value=point)
                qr = qr_state[:] + [qr_ancilla[i_compare]]  # add ancilla as compare qubit
                qr_remaining_ancilla = qr_ancilla[i_compare + 1:]  # take remaining ancillas
                
                self.append(comp.to_gate(),
                            qr[:] + qr_remaining_ancilla[:comp.num_ancillas])
                lin_r = LinearPauliRotations(num_state_qubits=self.num_state_qubits,
                                             slope=self.mapped_slopes[i],
                                             offset=self.mapped_offsets[i],
                                             basis=self.basis)
                self.append(lin_r.to_gate().control(),
                            [qr_ancilla[i_compare]] + qr_state[:] + qr_target)
                self.append(comp.to_gate().inverse(),
                            qr[:] + qr_remaining_ancilla[:comp.num_ancillas])
# with unqomp
def contains_zero_breakpoint(self) -> bool:
        return np.isclose(0, self.breakpoints[0])
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
    for i, point in enumerate(breakpoints):
        if i == 0 and _contains_zero_breakpoint(breakpoints):
            lin_r = LinearPauliRotations(num_state_qubits=num_state_qubits,
                                         slope=mapped_slopes[i],
                                         offset=mapped_offsets[i], basis = 'Y')
            circuit.append(lin_r.to_gate(), qr_state[:] + [qr_target])
        else:
            comp_ancilla = circuit.new_ancilla_register(1, name = 'ac' + str(i))
            circuit.append(makeIntegerComparator(num_state_qubits, point).to_ancilla_gate(), [*qr_state[:], comp_ancilla[0]])
            lin_r = LinearPauliRotations(num_state_qubits=num_state_qubits,
                                         slope=mapped_slopes[i],
                                         offset=mapped_offsets[i],
                                         basis=basis)
            circuit.append(lin_r.to_gate().control(),
                        [comp_ancilla[0]] + qr_state[:] + [qr_target])
    return circuit
