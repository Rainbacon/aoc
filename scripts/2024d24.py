import os

literals = {}
gates = {}
with open("data/2024/24/input.txt") as f:
    i = f.read()
    lines = i.split("\n\n")
    for line in lines[0].split("\n"):
        parts = line.split(": ")
        literals[parts[0]] = parts[1]
    
    for line in lines[1].split("\n"):
        parts = line.split(" -> ")
        gates[parts[1]] = parts[0].split(" ")

def find_gate(pred):
    for k,v in gates.items():
        if pred(v):
            return k

def combo(gate, in1, in2):
    pred1 = gate[0] == in1 and gate[2] == in2
    pred2 = gate[0] == in2 and gate[2] == in1
    return pred1 or pred2

def includes(gate, inp):
    return gate[0] == inp or gate[2] == inp

adders = []
broken = []
adders.append({
    "inputs": ["x00", "y00"],
    "output": "z00",
    "carry": find_gate(lambda g: g[1] == "AND" and combo(g, "x00", "y00"))
})

for i in range(1, 45):
    in1 = "x%02d" % i
    in2 = "y%02d" % i
    out = "z%02d" % i
    adder = {
        "inputs": [in1, in2],
        "output": out
    }
    intermediateAdd = find_gate(lambda g: g[1] == "XOR" and combo(g, in1, in2))
    intermediateCarry = find_gate(lambda g: g[1] == "AND" and combo(g, in1, in2))
    outputGate = gates[out]
    prevCarry = adders[i-1]["carry"]
    if not combo(outputGate, prevCarry, intermediateAdd):
        if not includes(outputGate, prevCarry):
            print(f"Adding {out} to broken because it does not include the previous carry")
            broken.append(out)
        elif not includes(outputGate, intermediateAdd):
            print(f"Adding {intermediateAdd} to broken because it is not included in the output gate")
            broken.append(intermediateAdd)
    
    intcarry2 = find_gate(lambda g: g[1] == "AND" and combo(g, prevCarry, intermediateAdd))
    carryOut = find_gate(lambda g: g[1] == "OR" and includes(g, intermediateCarry))
    if carryOut is None:
        print(f"Adding {intermediateCarry} to broken because it is not included in the carry out")
        broken.append(intermediateCarry)
    elif intcarry2 is not None:
        carryGate = gates[carryOut]
        if not combo(carryGate, intcarry2, intermediateCarry):
            print(f"Adding {intcarry2} to broken because the carry gate does not include it")
            broken.append(intcarry2)

    if carryOut is not None:
        adder["carry"] = carryOut
    else:
        adder["carry"] = find_gate(lambda g: g[1] == "OR" and (includes(g, intcarry2) or includes(g, intermediateAdd)))
    adders.append(adder)

print(broken)