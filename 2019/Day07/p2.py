lst, param, ptr, skip = [int(i) for i in open('input.txt').read().split(',')], {}, 0, (4,4,2,2,3,3,4,4)
while lst[ptr] is not 99:
    print("opcode " + str(lst[ptr]))
    opcode = lst[ptr] % 100
    for i in range(1,4): param[i] = ptr + i if lst[ptr]//int('100'.ljust(i+2, '0')) % 10 else lst[ptr + i]
    if opcode is 1: lst[param[3]] = lst[param[1]] + lst[param[2]]
    elif opcode is 2: lst[param[3]] = lst[param[1]] * lst[param[2]]
    elif opcode is 3: lst[param[1]] = 1 #int(input('1 or 5?: '))
    elif opcode is 4: print("out " +str(lst[param[1]]))
    elif opcode is 5 and lst[param[1]] or opcode is 6 and not lst[param[1]]: ptr = lst[param[2]] - 3
    elif opcode is 7: lst[param[3]] = 1 if lst[param[1]] < lst[param[2]] else 0
    elif opcode is 8: lst[param[3]] = 1 if lst[param[1]] == lst[param[2]] else 0
    ptr += skip[opcode-1]

