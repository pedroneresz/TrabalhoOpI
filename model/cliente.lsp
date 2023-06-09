(defstruct cliente
  nome
  cpf
  cnpj
  tipoCliente
  endereco
  telefone)

(defvar *clientes* nil)

(defun cadastrar-cliente ()
  (let ((novo-cliente (make-cliente)))
    (setf (cliente-nome novo-cliente) (read-from-minibuffer "Nome: "))
    (setf (cliente-cpf novo-cliente) (read-from-minibuffer "CPF: "))
    (setf (cliente-cnpj novo-cliente) (read-from-minibuffer "CNPJ: "))
    (setf (cliente-endereco novo-cliente) (read-from-minibuffer "Endereco: "))
    (setf (cliente-telefone novo-cliente) (read-from-minibuffer "Telefone: "))
    novo-cliente))

(defun adicionar-cliente ()
  (push (cadastrar-cliente) *clientes*))

(defun buscar-cliente-por-cpf (cpf)
  (find-if #'(lambda (cliente)
               (string= cpf (cliente-cpf cliente)))
           *clientes*))

(defun buscar-cliente-por-cnpj (cnpj)
  (find-if #'(lambda (cliente)
               (string= cnpj (cliente-cnpj cliente)))
           *clientes*))

(defun alterar-cliente ()
  (let ((cpf (read-from-minibuffer "Informe o CPF do cliente a ser alterado: ")))
    (let ((cliente (buscar-cliente-por-cpf cpf)))
      (if cliente
          (progn
            (setf (cliente-nome cliente) (read-from-minibuffer "Novo Nome: "))
            (setf (cliente-cpf cliente) (read-from-minibuffer "Novo CPF: "))
            (setf (cliente-endereco cliente) (read-from-minibuffer "Novo Endereco: "))
            (setf (cliente-telefone cliente) (read-from-minibuffer "Novo Telefone: "))
            (format t "Cliente alterado com sucesso!~%"))
          (format t "Cliente com CPF ~a não encontrado.~%" cpf)))))

(defun excluir-cliente ()
  (let ((cpf (read-from-minibuffer "Informe o CPF do cliente a ser excluído: ")))
    (let ((cliente (buscar-cliente-por-cpf cpf)))
      (if cliente
          (progn
            (setf *clientes* (delete cliente *clientes*))
            (format t "Cliente excluído com sucesso!~%"))
        (format t "Cliente com CPF ~a não encontrado.~%" cpf)))))

(defun listar-clientes ()
  (if (null *clientes*)
      (format t "Não há clientes cadastrados!.~%")
      (dolist (cliente *clientes*)
        (format t "Nome: ~a~%CPF: ~a~%CNPJ: ~a~%Endereco: ~a~%Telefone: ~a~%~%"
                (cliente-nome cliente)
                (cliente-cpf cliente)
                (cliente-cnpj cliente)
                (cliente-endereco cliente)
                (cliente-telefone cliente)))))

