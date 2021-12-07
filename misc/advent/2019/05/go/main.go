package main

import (
	"io/ioutil"
	"log"
	"os"
	"strconv"
	"strings"
	"time"
)

const (
	ADD    = "1"
	MUL    = "2"
	INPUT  = "3"
	OUTPUT = "4"
	JIT    = "5"
	JIF    = "6"
	LESS   = "7"
	EQ     = "8"
	QUIT   = "99"

	ModePos = '0' // position mode
	ModeImm = '1' // immediate mode

	DEBUG = true
)

func cpu(mem []int, input []string) {
	ip := 0
	ops := 0
	start := time.Now()
	params := []byte{'0', '0', '0'}
execLoop:
	for {
		ops++
		pop := strconv.Itoa(mem[ip])

		// pull the  params out
		l := len(pop)
		i := l - 3
		p := 0
		for i >= 0 {
			params[p] = pop[i]
			p++
			i--
		}
		for p < 3 {
			params[p] = '0'
			p++
		}

		op := ""
		if l > 1 && pop[l-2] != '0' {
			op = pop[l-2 : l]
		} else {
			op = string(pop[l-1])
		}

		switch op {
		case QUIT:
			break execLoop
		case ADD:
			a := mem[ip+1]
			if params[0] == ModePos {
				a = mem[a]
			}
			b := mem[ip+2]
			if params[1] == ModePos {
				b = mem[b]
			}

			out := mem[ip+3]
			mem[out] = a + b
			if DEBUG {
				log.Printf("ADD %d(%d) %d(%d) %d(%d)", mem[ip+1], a, mem[ip+2], b, out, mem[out])
			}
			ip += 4
		case MUL:
			a := mem[ip+1]
			if params[0] == ModePos {
				a = mem[a]
			}
			b := mem[ip+2]
			if params[1] == ModePos {
				b = mem[b]
			}

			out := mem[ip+3]
			mem[out] = a * b
			if DEBUG {
				log.Printf("MUL %d(%d) %d(%d) %d(%d)", mem[ip+1], a, mem[ip+2], b, out, mem[out])
			}
			ip += 4
		case INPUT:
			loc := mem[ip+1]
			if params[0] == ModeImm {
				log.Fatalf("Unexpected immediate mode input %s", pop)
			}

			val, newinput := input[0], input[1:]
			input = newinput // annoyingly we can't do this as part of the assignment above
			ival, err := strconv.Atoi(val)
			if err != nil {
				log.Fatalf("Unable to convert %s", val)
			}
			mem[loc] = ival
			ip += 2
			if DEBUG {
				log.Printf("INPUT %d %d", loc, mem[loc])
			}
		case OUTPUT:
			loc := mem[ip+1]
			if params[0] == ModePos {
				loc = mem[loc]
			}

			log.Printf("> %d", loc)
			ip += 2
		case JIT:
			test := mem[ip+1]
			if params[0] == ModePos {
				test = mem[test]
			}
			val := mem[ip+2]
			if params[1] == ModePos {
				val = mem[val]
			}
			ip += 3

			if test != 0 {
				ip = val
			}
			if DEBUG {
				log.Printf("JIT %d %d %d %s", test, val, ip, params)
			}
		case JIF:
			test := mem[ip+1]
			if params[0] == ModePos {
				test = mem[test]
			}
			val := mem[ip+2]
			if params[1] == ModePos {
				val = mem[val]
			}
			ip += 3

			if test == 0 {
				ip = val
			}
			if DEBUG {
				log.Printf("JIF %d %d %d", test, val, ip)
			}
		case LESS:
			a := mem[ip+1]
			if params[0] == ModePos {
				a = mem[a]
			}
			b := mem[ip+2]
			if params[1] == ModePos {
				b = mem[b]
			}

			if a < b {
				mem[mem[ip+3]] = 1
			} else {
				mem[mem[ip+3]] = 0
			}
			if DEBUG {
				log.Printf("LESS %d %d %d", a, b, mem[mem[ip+3]])
			}

			ip += 4
		case EQ:
			a := mem[ip+1]
			if params[0] == ModePos {
				a = mem[a]
			}
			b := mem[ip+2]
			if params[1] == ModePos {
				b = mem[b]
			}

			if a == b {
				mem[mem[ip+3]] = 1
			} else {
				mem[mem[ip+3]] = 0
			}
			if DEBUG {
				log.Printf("EQ %d %d %d", a, b, mem[mem[ip+3]])
			}

			ip += 4
		default:
			log.Fatalf("Invalid op %s", op)
		}
	}

	bananas
	dur := time.Now().Sub(start)
	log.Printf("run took %.04fs for %d ops [%.0f ops/s]", dur.Seconds(), ops, float64(ops)/dur.Seconds())
}

func main() {
	if len(os.Args) < 2 {
		log.Fatalf("Usage: cpu <input file> [<input buffer 1>, <input buffer 2>...]")
	}
	infile := os.Args[1]
	inbuf := os.Args[2:]

	program, err := ioutil.ReadFile(infile)
	if err != nil {
		log.Fatalf("Unable to read file %s <%s>", infile, err)
	}

	memory := []int{}
	prog := strings.Split(strings.Trim(string(program), "\n"), ",")
	for _, inst := range prog {
		insti, err := strconv.Atoi(inst)
		if err != nil {
			log.Fatalf("Unable to convert int %s %s", inst, err)
		}
		memory = append(memory, insti)
	}

	cpu(memory, inbuf)
}
