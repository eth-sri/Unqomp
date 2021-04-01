from qiskit import QuantumCircuit,  QuantumRegister
from unqomp.ancillaallocation import AncillaCircuit
from unqomp.examples.quipperadder import makesAdder
from unqomp.examples.adder import makesMult
from unqomp.examples.weightedadder import makeWeightedAdder, makeWeightedAdderWOExtraCtrlsQb
from unqomp.examples.intergercomparator import makeIntegerComparator
from unqomp.examples.deutschjozsa import makesDJ

def mcx():
    x = QuantumRegister(10)
    r = QuantumRegister(1)
    circuit = AncillaCircuit(x, r)
    circuit.mcx(x[:], r)
    circuit = circuit.circuitWithUncomputation()
    nb_qb_mi = circuit.num_qubits
    nb_gates_mi = circuit.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("MCX " + str(nb_qb_mi) + ';' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))


def outOfPlaceAdder():
    circuit1 = makesAdder(4).circuitWithUncomputation()
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("Adder " + str(nb_qb_mi) + ';' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

def mult(): 
    circuit1 = makesMult(4).circuitWithUncomputation()

    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("Multiplier " + str(nb_qb_mi) + ';' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

def weightedAdder():
    circuit1 = makeWeightedAdder(4, [15, 15, 15, 15]).circuitWithUncomputation()
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("WeightedAdder " + str(nb_qb_mi) + ';' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

def weightedAdderAltImpl():
    circuit1 = makeWeightedAdderWOExtraCtrlsQb(4, [15, 15, 15, 15]).circuitWithUncomputation()
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("WeightedAdder alt. impl. " + str(nb_qb_mi) + ';' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

def integerComp():
    circuit1 = makeIntegerComparator(4, 4).circuitWithUncomputation()
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("IntegerComparator " + str(nb_qb_mi) + ';' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

def DJ():
    (circuit1, varreg) = makesDJ(10)
    circuit1 = circuit1.circuitWithUncomputation()
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("Deutsch-Jozsa " + str(nb_qb_mi) + ';' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

def grover():
    import unqomp.examples.grover as grover
    (circuit1, working_bits) = grover.makesGroverCircuit(10)
    circuit1 = circuit1.circuitWithUncomputation()
    nb_qb_mi = circuit1.num_qubits
    nb_gates_mi = circuit1.decompose().decompose().decompose().decompose().decompose().count_ops()
    print("Grover " + str(nb_qb_mi) + ';' + str(nb_gates_mi['cx'] + nb_gates_mi['u3']) + ';' + str(nb_gates_mi['cx']))

print("Example name  ;  number of qubits qith Unqomp  ;  number of gates with Unqomp  ;  number of CX gates with Unqomp")
outOfPlaceAdder()
DJ()
grover()
integerComp()
mcx()
mult()
weightedAdder()
weightedAdderAltImpl()
