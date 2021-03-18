# taken from https://github.com/Qiskit/qiskit-terra/blob/d7e469e33c212e8acf39308497bd312cfe792ca6/qiskit/circuit/library/standard_gates/multi_control_rotation_gates.py
def mcry(self, theta, q_controls, q_target, q_ancillae, mode='basic', use_basis_gates=False):
    all_qubits = control_qubits + [target_qubit] + ancillary_qubits
		# added 4 lines of init regs and defs, hidden in parent class
    self.u3(theta / 2, 0, 0, q_target)
    self.mct(q_controls, q_target, q_ancillae)
    self.u3(-theta / 2, 0, 0, q_target)
    self.mct(q_controls, q_target, q_ancillae)

#mcry with uncomp
def mcry():
		circuit.mcx(q_controls[:], q_ancilla)
    circuit.cry(theta, q_ancilla, q_target)
		return circuit.circuitWithUncomputation()
 
