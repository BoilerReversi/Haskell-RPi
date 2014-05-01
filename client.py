import socket
import sys

HOST = '10.184.47.67'
PORT = 5678

try:
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((sys.argv[1], PORT))
#    s.send("-------------------X-------XX------XO--------------------------- O"+"\n")
    s.send(sys.argv[2]+"\n\n")
    
    data = s.recv(2048)
    data1 = s.recv(2048)
    s.close()
    print data+data1
except:
    print "J"
