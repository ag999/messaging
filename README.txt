Before our code can be compiled the required dependencies must be installed.

First, execute "opam install async" if Async is not installed on your machine.
This will install Jane Street's Async library as well as all it's dependencies.

Next, do "opam install zarith" if Zarith is not installed on your machine. Some
of our group had to install gmp because we got an error when attempting to
install Zarith. To do this use either "sudo port install gmp" or "brew install gmp".

Now you need to compile each of our modules. To do this use the following three
commands in terminal:

corebuild -pkgs async,zarith RSA.byte
corebuild -pkgs async,zarith server.byte
corebuild -pkgs async,zarith client.byte

You're now ready to run our chatroom! Open at least 3 terminal windows from
within our project folder. In one of the windows run "./server.byte", and in
the other two run "./client.byte". Now in each of the client windows you can
follow the instructions displayed to create a username and start a chat.

For instructions on what built-in commands exist, simply type "//help" at any
time. To join a chat simply type in the user you'd like to connect with from
the home screen of the chatroom. Both users must request to connect to each other
in order to see each other's messages.

If you'd like to run our test cases, simply compile using:

ocamlbuild -pkgs ounit,zarith RSAtests.byte

And then run via "./RSAtests.byte"
