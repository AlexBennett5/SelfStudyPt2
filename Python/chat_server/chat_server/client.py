from socket import socket, AF_INET, SOCK_STREAM
from threading import Thread
import sys
import tkinter as tk

BUFFER_SIZE = 4096


class Client:

    def __init__(self):
        self.top = tk.Tk()
        self.top.title("ChatApp")
        self.msg_frame = tk.Frame(self.top)
        self.msg = tk.StringVar()
        self.msg.set("Your messages here.")
        self.scrollbar = tk.Scrollbar(self.msg_frame)
        self.msg_list = tk.Listbox(self.msg_frame, height=15, width=50, yscrollcommand=self.scrollbar.set)
        self.scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        self.msg_list.pack(side=tk.LEFT, fill=tk.BOTH)
        self.msg_list.pack()
        self.msg_frame.pack()
        self.entry_field = tk.Entry(self.top, textvariable=self.msg)
        self.entry_field.bind('<Return>', self.send_message)
        self.entry_field.pack()
        self.send_button = tk.Button(self.top, text='Send', command=self.send_message)
        self.send_button.pack()
        self.top.protocol('WM_DELETE_WINDOW', self.on_close)

    def connect_to(self, hostname, portno):
        self.client_socket = socket(AF_INET, SOCK_STREAM)
        self.client_socket.connect((hostname, portno))
        receive_thread = Thread(target=self.receive_loop)
        receive_thread.start()
        tk.mainloop()

    def receive_loop(self):
        while True:
            try:
                message = self.client_socket.recv(BUFFER_SIZE).decode('utf8')
                self.msg_list.insert(tk.END, message)
            except OSError:
                break

    def send_message(self, event=None):
        message = self.msg.get()
        self.msg.set('')
        self.client_socket.send(bytes(message, 'utf8'))
        if message == '{quit}':
            self.client_socket.close()
            self.top.quit()

    def on_close(self, event=None):
        self.msg.set('{quit}')
        self.send_message()


if __name__ == '__main__':
    if (len(sys.argv) < 3):
        print('Format requires: python client.py hostname portno')
        sys.exit()

    client = Client()
    client.connect_to(sys.argv[1], int(sys.argv[2]))
