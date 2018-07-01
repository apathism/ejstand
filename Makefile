
SOURCES = \
	Main.hs \
	DataParser.hs \
	BaseModels.hs \
	StandingsModels.hs
OBJECTS = $(SOURCES:.cpp=.o)
EXECUTABLE = ejstand

all: $(SOURCES) $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	ghc -dynamic $(OBJECTS) -o $@

clean_all: clean
	$(RM) ejstand

clean:
	$(RM) *.hi *.o

.hs.o:
	ghc -prof -dynamic -c $< -o $@
