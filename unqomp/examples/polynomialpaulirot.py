# Adapted from https://qiskit.org/documentation/_modules/qiskit/circuit/library/arithmetic/polynomial_pauli_rotations.html#PolynomialPauliRotations

from itertools import product

from qiskit.circuit import QuantumRegister, QuantumCircuit
from qiskit.circuit.exceptions import CircuitError
from unqomp.ancillaallocation import AncillaCircuit


def _binomial_coefficients(n):
    """"Return a dictionary of binomial coefficients

    Based-on/forked from sympy's binomial_coefficients() function [#]

    .. [#] https://github.com/sympy/sympy/blob/sympy-1.5.1/sympy/ntheory/multinomial.py
    """

    data = {(0, n): 1, (n, 0): 1}
    temp = 1
    for k in range(1, n // 2 + 1):
        temp = (temp * (n - k + 1)) // k
        data[k, n - k] = data[n - k, k] = temp
    return data


def _large_coefficients_iter(m, n):
    """"Return an iterator of multinomial coefficientss

    Based-on/forked from sympy's multinomial_coefficients_iterator() function [#]

    .. [#] https://github.com/sympy/sympy/blob/sympy-1.5.1/sympy/ntheory/multinomial.py
    """
    if m < 2*n or n == 1:
        coefficients = _multinomial_coefficients(m, n)
        for key, value in coefficients.items():
            yield(key, value)
    else:
        coefficients = _multinomial_coefficients(n, n)
        coefficients_dict = {}
        for key, value in coefficients.items():
            coefficients_dict[tuple(filter(None, key))] = value
        coefficients = coefficients_dict

        temp = [n] + [0] * (m - 1)
        temp_a = tuple(temp)
        b = tuple(filter(None, temp_a))
        yield (temp_a, coefficients[b])
        if n:
            j = 0  # j will be the leftmost nonzero position
        else:
            j = m
        # enumerate tuples in co-lex order
        while j < m - 1:
            # compute next tuple
            temp_j = temp[j]
            if j:
                temp[j] = 0
                temp[0] = temp_j
            if temp_j > 1:
                temp[j + 1] += 1
                j = 0
            else:
                j += 1
                temp[j] += 1

            temp[0] -= 1
            temp_a = tuple(temp)
            b = tuple(filter(None, temp_a))
            yield (temp_a, coefficients[b])


def _multinomial_coefficients(m, n):
    """"Return an iterator of multinomial coefficientss

    Based-on/forked from sympy's multinomial_coefficients() function [#]

    .. [#] https://github.com/sympy/sympy/blob/sympy-1.5.1/sympy/ntheory/multinomial.py
    """
    if not m:
        if n:
            return {}
        return {(): 1}
    if m == 2:
        return _binomial_coefficients(n)
    if m >= 2*n and n > 1:
        return dict(_large_coefficients_iter(m, n))
    if n:
        j = 0
    else:
        j = m
    temp = [n] + [0] * (m - 1)
    res = {tuple(temp): 1}
    while j < m - 1:
        temp_j = temp[j]
        if j:
            temp[j] = 0
            temp[0] = temp_j
        if temp_j > 1:
            temp[j + 1] += 1
            j = 0
            start = 1
            v = 0
        else:
            j += 1
            start = j + 1
            v = res[tuple(temp)]
            temp[j] += 1
        for k in range(start, m):
            if temp[k]:
                temp[k] -= 1
                v += res[tuple(temp)]
                temp[k] += 1
        temp[0] -= 1
        res[tuple(temp)] = (v * temp_j) // (n - temp[0])
    return res


def makesPolyPauliRot(num_state_qubits = None, coeffs = None):
    # basis is y
    degree = len(coeffs) - 1
    
    qr_state = QuantumRegister(num_state_qubits, name='state')
    qr_target = QuantumRegister(1, name='target')
    
    def _get_rotation_coefficients():
        """Compute the coefficient of each monomial.

        Returns:
            A dictionary with pairs ``{control_state: rotation angle}`` where ``control_state``
            is a tuple of ``0`` or ``1`` bits.
        """
        # determine the control states
        all_combinations = list(product([0, 1], repeat=num_state_qubits))
        valid_combinations = []
        for combination in all_combinations:
            if 0 < sum(combination) <= degree:
                valid_combinations += [combination]

        rotation_coeffs = {control_state: 0 for control_state in valid_combinations}

        # compute the coefficients for the control states
        for i, coeff in enumerate(coeffs[1:]):
            i += 1  # since we skip the first element we need to increase i by one

            # iterate over the multinomial coefficients
            for comb, num_combs in _multinomial_coefficients(num_state_qubits, i).items():
                control_state = ()
                power = 1
                for j, qubit in enumerate(comb):
                    if qubit > 0:  # means we control on qubit i
                        control_state += (1,)
                        power *= 2 ** (j * qubit)
                    else:
                        control_state += (0,)

                # Add angle
                rotation_coeffs[control_state] += coeff * num_combs * power

        return rotation_coeffs

    circuit = AncillaCircuit(qr_state, qr_target)
    rotation_coeffs = _get_rotation_coefficients()

    circuit.ry(coeffs[0], qr_target)
    
    for c in rotation_coeffs:
        qr_control = []
        for i, _ in enumerate(c):
            if c[i] > 0:
                qr_control.append(qr_state[i])

        # apply controlled rotations
        if len(qr_control) > 1:
            circuit.mcry(rotation_coeffs[c], qr_control, qr_target)
        elif len(qr_control) == 1:
            circuit.cry(rotation_coeffs[c], qr_control[0], qr_target)

    return circuit

def makesQiskitPolyPauliRot(num_state_qubits = None, coeffs = None):
    # Qiskit implementation still has a regression bug: mcry uses the default 'noancilla' mode instead of basic when asked for it
    from unqomp.examples.mcx import makeQiskitMCRY
    degree = len(coeffs) - 1
    num_ancilla_qubits = max(1, degree - 1)
    
    qr_state = QuantumRegister(num_state_qubits, name='state')
    qr_target = QuantumRegister(1, name='target')
    qr_ancilla = QuantumRegister(num_ancilla_qubits, name='ancilla')
    
    def _get_rotation_coefficients():
        """Compute the coefficient of each monomial.

        Returns:
            A dictionary with pairs ``{control_state: rotation angle}`` where ``control_state``
            is a tuple of ``0`` or ``1`` bits.
        """
        # determine the control states
        all_combinations = list(product([0, 1], repeat=num_state_qubits))
        valid_combinations = []
        for combination in all_combinations:
            if 0 < sum(combination) <= degree:
                valid_combinations += [combination]

        rotation_coeffs = {control_state: 0 for control_state in valid_combinations}

        # compute the coefficients for the control states
        for i, coeff in enumerate(coeffs[1:]):
            i += 1  # since we skip the first element we need to increase i by one

            # iterate over the multinomial coefficients
            for comb, num_combs in _multinomial_coefficients(num_state_qubits, i).items():
                control_state = ()
                power = 1
                for j, qubit in enumerate(comb):
                    if qubit > 0:  # means we control on qubit i
                        control_state += (1,)
                        power *= 2 ** (j * qubit)
                    else:
                        control_state += (0,)

                # Add angle
                rotation_coeffs[control_state] += coeff * num_combs * power

        return rotation_coeffs

    circuit = QuantumCircuit(qr_state, qr_target, qr_ancilla)
    rotation_coeffs = _get_rotation_coefficients()

    circuit.ry(coeffs[0], qr_target)
    
    for c in rotation_coeffs:
        qr_control = []
        for i, _ in enumerate(c):
            if c[i] > 0:
                qr_control.append(qr_state[i])

        # apply controlled rotations
        if len(qr_control) > 1:
            circuit.append(makeQiskitMCRY(rotation_coeffs[c], len(qr_control)), [*qr_control, qr_target, *qr_ancilla[:len(qr_control) - 2]])
        elif len(qr_control) == 1:
            circuit.cry(rotation_coeffs[c], qr_control[0], qr_target)

    return circuit
