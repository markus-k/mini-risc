#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define MIN(X, Y) (((X) < (Y)) ? (X) : (Y))
#define BYTE_TO_BINARY_PATTERN "%c%c%c%c%c%c%c%c"
#define BYTE_TO_BINARY(byte)  \
  (byte & 0x80 ? '1' : '0'), \
  (byte & 0x40 ? '1' : '0'), \
  (byte & 0x20 ? '1' : '0'), \
  (byte & 0x10 ? '1' : '0'), \
  (byte & 0x08 ? '1' : '0'), \
  (byte & 0x04 ? '1' : '0'), \
  (byte & 0x02 ? '1' : '0'), \
  (byte & 0x01 ? '1' : '0')

#define LINE_LEN 120
#define PROGRAM_LEN 1000
#define LABELS_LEN 100
#define LABEL_LEN 64

#define REG_SP 0xD
#define REG_FLAGS 0xE
#define REG_PC 0xF

#define OPCODE_MOV 0x0
#define OPCODE_MOVIL 0x1
#define OPCODE_MOVIH 0x2
#define OPCODE_ADD 0x3
#define OPCODE_SUB 0x4
#define OPCODE_AND 0x5
#define OPCODE_OR 0x6
#define OPCODE_XOR 0x7
#define OPCODE_SHT 0x8
#define OPCODE_JMPRI 0x9
#define OPCODE_CMP 0xa
#define OPCODE_JMPC 0xb
#define OPCODE_LDR 0xc
#define OPCODE_STR 0xd
#define OPCODE_PUSH 0xe
#define OPCODE_POP 0xf

struct instr {
    uint8_t opcode;
    uint8_t dst;
    uint8_t src1;
    uint8_t src2;
    uint16_t imm;
    char label[LABEL_LEN];
};

struct label {
    char name[LABEL_LEN];
    int location;
};

#define STRTOK_SEP "\n\t\r ,"

uint8_t parse_reg(char *arg) {
    uint32_t reg = 0;

    if (strcmp(arg, "sp") == 0) {
        reg = REG_SP;
    } else if (strcmp(arg, "flags") == 0) {
        reg = REG_FLAGS;
    } else if (strcmp(arg, "pc") == 0) {
        reg = REG_PC;
    } else if (sscanf(arg, "r%u", &reg) != 1) {
        fprintf(stderr, "error: '%s' is not a valid register name\n", arg);
        exit(2);
    }

    return reg;
}

uint16_t parse_imm(char *arg, int width) {
    uint32_t imm = 0;

    if (arg[0] == '0' && arg[1] == 'x') {
        if (sscanf(arg, "0x%x", &imm) != 1) {
            fprintf(stderr, "error: failed to parse hex immediate '%s'\n", arg);
            exit(2);
        }
    } else {
        if (sscanf(arg, "%u", &imm) != 1) {
            fprintf(stderr, "error: failed to parse decimal immediate '%s'\n", arg);
            exit(2);
        }
    }

    if (imm > (1 << width) - 1) {
        fprintf(stderr, "error: immediate '%u' is too wide for imm%d", imm, width);
        exit(2);
    }

    return imm;
}

uint8_t parse_cond(char *arg) {
    if (strcmp(arg, "al") == 0) {
        return 0x0;
    } else if (strcmp(arg, "eq") == 0) {
        return 0x1;
    } else if (strcmp(arg, "ne") == 0) {
        return 0x2;
    } else if (strcmp(arg, "hs") == 0) {
        return 0x3;
    } else if (strcmp(arg, "lo") == 0) {
        return 0x4;
    } else if (strcmp(arg, "mi") == 0) {
        return 0x5;
    } else if (strcmp(arg, "po") == 0) {
        return 0x6;
    } else if (strcmp(arg, "vs") == 0) {
        return 0x7;
    } else if (strcmp(arg, "vc") == 0) {
        return 0x8;
    } else if (strcmp(arg, "hi") == 0) {
        return 0x9;
    } else if (strcmp(arg, "ls") == 0) {
        return 0xa;
    } else if (strcmp(arg, "ge") == 0) {
        return 0xb;
    } else if (strcmp(arg, "lt") == 0) {
        return 0xc;
    } else if (strcmp(arg, "gt") == 0) {
        return 0xd;
    } else if (strcmp(arg, "le") == 0) {
        return 0xe;
    } else {
        fprintf(stderr, "error: invalid condition code '%s'\n", arg);
        exit(2);
    }

    return 0x0;
}

int main(int argc, char **argv) {
    const char *asm_filename;
    const char *out_filename;
    FILE *fd;
    char line[LINE_LEN];
    char *token;
    char *op1;
    char *op2;
    char *op3;
    char *op4;
    int counter = 0;
    int labelcounter = 0;
    struct instr program[PROGRAM_LEN];
    struct label labels[LABELS_LEN];
    memset(program, 0, sizeof(program));
    memset(labels, 0, sizeof(labels));

    if (argc < 3) {
        printf("usage: %s ASM_FILE OUT_FILE\n", argv[0]);
        return 1;
    }

    asm_filename = argv[1];
    out_filename = argv[2];

    fd = fopen(asm_filename, "r");
    if (fd == NULL) {
        perror("Opening assembly file failed");
        return 1;
    }

    /****** parsing *******/

    while (fgets(line, sizeof(line), fd) != NULL) {
        token = strtok(line, STRTOK_SEP);

        if (token && token[0] == '#') {
            // comment, ignore
            continue;
        }

        while (token) {
            if (strcmp(token, "mov") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_MOV;
                program[counter].dst = parse_reg(op1);
                program[counter].src1 = parse_reg(op2);
                counter++;
            } else if (strcmp(token, "movil") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_MOVIL;
                program[counter].dst = parse_reg(op1);
                program[counter].imm = parse_imm(op2, 8);
                counter++;
            } else if (strcmp(token, "movih") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_MOVIH;
                program[counter].dst = parse_reg(op1);
                program[counter].imm = parse_imm(op2, 8);
                counter++;
            } else if (strcmp(token, "add") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);
                op3 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_ADD;
                program[counter].dst = parse_reg(op1);
                program[counter].src1 = parse_reg(op2);
                program[counter].src2 = parse_reg(op3);
                counter++;
            } else if (strcmp(token, "sub") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);
                op3 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_SUB;
                program[counter].dst = parse_reg(op1);
                program[counter].src1 = parse_reg(op2);
                program[counter].src2 = parse_reg(op3);
                counter++;
            } else if (strcmp(token, "and") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);
                op3 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_AND;
                program[counter].dst = parse_reg(op1);
                program[counter].src1 = parse_reg(op2);
                program[counter].src2 = parse_reg(op3);
                counter++;
            } else if (strcmp(token, "or") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);
                op3 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_OR;
                program[counter].dst = parse_reg(op1);
                program[counter].src1 = parse_reg(op2);
                program[counter].src2 = parse_reg(op3);
                counter++;
            } else if (strcmp(token, "xor") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);
                op3 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_XOR;
                program[counter].dst = parse_reg(op1);
                program[counter].src1 = parse_reg(op2);
                program[counter].src2 = parse_reg(op3);
                counter++;
            } else if (strcmp(token, "sht") == 0) {
                uint8_t shift = 0;
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);
                op3 = strtok(NULL, STRTOK_SEP);
                op4 = strtok(NULL, STRTOK_SEP);

                if (strlen(op3) > 1 || (op3[0] != 'l' && op3[0] != 'r')) {
                    fprintf(stderr, "error: invalid shift direction '%s'\n", op3);
                    exit(2);
                }

                if (op3[0] == 'r') {
                    shift |= (1 << 3);
                }

                program[counter].opcode = OPCODE_SHT;
                program[counter].dst = parse_reg(op1);
                program[counter].src1 = parse_reg(op2);
                program[counter].imm = shift | parse_imm(op4, 3);
                counter++;
            } else if (strcmp(token, "jmpri") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_JMPRI;
                program[counter].imm = parse_imm(op1, 12);

                // TODO: label support

                counter++;
            } else if (strcmp(token, "cmp") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_CMP;
                program[counter].src1 = parse_reg(op1);
                program[counter].src2 = parse_reg(op2);
                counter++;
            } else if (strcmp(token, "jmpc") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_JMPC;
                program[counter].dst = parse_reg(op2);
                program[counter].imm = parse_cond(op1);

                // TODO: label support

                counter++;
            } else if (strcmp(token, "ldr") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_LDR;
                program[counter].dst = parse_reg(op1);
                program[counter].src1 = parse_reg(op2);
                counter++;
            } else if (strcmp(token, "str") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);
                op2 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_STR;
                program[counter].dst = parse_reg(op1);
                program[counter].src1 = parse_reg(op2);
                counter++;
            } else if (strcmp(token, "push") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_PUSH;
                program[counter].src1 = parse_reg(op1);
                counter++;
            } else if (strcmp(token, "pop") == 0) {
                op1 = strtok(NULL, STRTOK_SEP);

                program[counter].opcode = OPCODE_POP;
                program[counter].dst = parse_reg(op1);
                counter++;
            } else {
                size_t len = strlen(token);

                if (token[len - 1] == ':') {
                    // ends with colon, is a label
                    labels[labelcounter].location = counter;
                    // copy name without colon
                    strncpy(labels[labelcounter].name, token, MIN(LABEL_LEN, len - 1));
                    labelcounter++;
                } else {
                    fprintf(stderr, "error: unkown instruction '%s'\n", token);
                    exit(2);
                }
            }

            token = strtok(NULL, STRTOK_SEP);
        }
    }

    fclose(fd);

    /****** resolve labels *******/

    // TODO

    printf("label table:\n");
    for (int i = 0; i < labelcounter; i++) {
        printf("%16s: 0x%.2x\n", labels[i].name, labels[i].location);
    }

    /****** generate machine code ******/

    fd = fopen(out_filename, "w");

    for (int i = 0; i < counter; i++) {
        uint16_t instr = (uint16_t)program[i].opcode << 12;
        uint8_t buf[2];

        switch (program[i].opcode) {
        case OPCODE_MOV:
        case OPCODE_ADD:
        case OPCODE_SUB:
        case OPCODE_AND:
        case OPCODE_OR:
        case OPCODE_XOR:
            // dst, src1, src2
            instr |= (uint16_t)program[i].dst << 8;
            instr |= (uint16_t)program[i].src1 << 4;
            instr |= (uint16_t)program[i].src2;
            break;
        case OPCODE_SHT:
            // dst, src1, src2
            instr |= (uint16_t)program[i].dst << 8;
            instr |= (uint16_t)program[i].src1 << 4;
            instr |= program[i].imm & 0xf;
            break;
        case OPCODE_MOVIL:
        case OPCODE_MOVIH:
            // dst, imm8
            instr |= (uint16_t)program[i].dst << 8;
            instr |= program[i].imm & 0xff;
            break;
        case OPCODE_JMPRI:
            // imm12
            instr |= program[i].imm & 0xfff;
            break;
        case OPCODE_CMP:
            // src1, src2
            instr |= (uint16_t)program[i].src1 << 4;
            instr |= (uint16_t)program[i].src2;
            break;
        case OPCODE_JMPC:
            instr |= (uint16_t)program[i].dst << 8;
            instr |= (program[i].imm & 0xf) << 4;
            break;
        case OPCODE_LDR:
        case OPCODE_STR:
            // dst, src1
            instr |= (uint16_t)program[i].dst << 8;
            instr |= (uint16_t)program[i].src1 << 4;
            break;
        case OPCODE_PUSH:
        case OPCODE_POP:
            instr |= (uint16_t)program[i].dst << 8;
            break;
        }

        buf[0] = instr >> 8;
        buf[1] = instr;

        //fprintf(fd, "%.2x\n", instr);
        fprintf(fd, BYTE_TO_BINARY_PATTERN BYTE_TO_BINARY_PATTERN "\n", BYTE_TO_BINARY(buf[0]), BYTE_TO_BINARY(buf[1]));
        //fwrite(&buf, 1, sizeof(buf), fd);
    }

    fclose(fd);
}
