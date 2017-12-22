package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/mcandre/tonixxx"
)

var flagDebug = flag.Bool("debug", false, "Enable debugging logs")
var flagVersion = flag.Bool("version", false, "Show version information")
var flagHelp = flag.Bool("help", false, "Show usage information")

const taskUp = "up"
const taskBoil = "boil"
const taskDown = "down"
const taskClean = "clean"

var taskNames = []string{taskUp, taskBoil, taskDown, taskClean}

func main() {
	flag.Parse()

	switch {
	case *flagVersion:
		fmt.Println(tonixxx.Version)
		os.Exit(0)
	case *flagHelp:
		flag.PrintDefaults()
		os.Exit(0)
	}

	tasks := flag.Args()

	if len(tasks) < 1 {
		log.Fatalf("Missing task names, try one of %v", taskNames)
	}

	configFilename, err := tonixxx.ConfigFile()

	if err != nil {
		log.Panic(err)
	}

	distilleryP, err := tonixxx.LoadDistillery(configFilename)

	if err != nil {
		log.Panic(err)
	}

	distillery := *distilleryP

	if *flagDebug {
		log.Printf("Loaded distillery: %v", distillery)
	}

	for _, task := range tasks {
		switch task {
		case taskUp:
			if err := distillery.Up(); err != nil {
				log.Panic(err)
			}
		case taskBoil:
			if err := distillery.Boil(); err != nil {
				log.Panic(err)
			}
		case taskDown:
			if err := distillery.Down(); err != nil {
				log.Panic(err)
			}
		case taskClean:
			if err := distillery.Clean(); err != nil {
				log.Panic(err)
			}
		default:
			log.Fatalf("Invalid task name, try one of %v", taskNames)
		}
	}
}
