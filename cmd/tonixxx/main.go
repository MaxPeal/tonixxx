package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"github.com/mcandre/tonixxx"
)

var flagVersion = flag.Bool("version", false, "Show version information")
var flagHelp = flag.Bool("help", false, "Show usage information")

const TASK_UP = "up"
const TASK_BOIL = "boil"
const TASK_DOWN = "down"
const TASK_CLEAN = "clean"

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

	configFilename, err := tonixxx.ConfigFile()

	if err != nil {
		log.Panic(err)
	}

	distilleryP, err := tonixxx.LoadDistillery(configFilename)

	if err != nil {
		log.Panic(err)
	}

	distillery := *distilleryP

	tasks := flag.Args()

	for _, task := range(tasks) {
		switch task {
		case TASK_UP:
			if err := distillery.Up(); err != nil {
				log.Panic(err)
			}
		case TASK_BOIL:
			if err := distillery.Boil(); err != nil {
				log.Panic(err)
			}
		case TASK_DOWN:
			if err := distillery.Down(); err != nil {
				log.Panic(err)
			}
		case TASK_CLEAN:
			if err := distillery.Clean(); err != nil {
				log.Panic(err)
			}
		}
	}
}
