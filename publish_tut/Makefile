FILES = Program.fs
PROJ_NAME = publish_tut

build: bin/Debug/netcoreapp3.1/$(PROJ_NAME)

run: build link
	./$(PROJ_NAME)

bin/Debug/netcoreapp3.1/$(PROJ_NAME): $(FILES)
	dotnet build --no-restore

$(PROJ_NAME):
	ln -s bin/Debug/netcoreapp3.1/$(PROJ_NAME)

link: $(PROJ_NAME)

publish: publish.tar.gz

publish.tar.gz:
	dotnet publish -o publish/
	tar -czvf publish.tar.gz publish/
	rm -r publish/

clean:
	dotnet clean
	dotnet clean -c Release

.PHONY:
	run build clean link publish
