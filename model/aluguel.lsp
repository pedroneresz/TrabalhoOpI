(defstruct aluguel
  dataSaida
  dataDevolucao
  status
  cpfCliente
  veiculo
  valorTotal
  diasAlugados)

(defvar *alugueis* nil)
(defvar *valor-diaria* 150)

(defun cadastrar-aluguel (cliente veiculo data-inicio data-fim)
  (let ((novo-aluguel (make-aluguel :dataSaida data-inicio
                                    :dataDevolucao data-fim
                                    :cpfCliente (cliente-cpf cliente)
                                    :veiculo (veiculo-placa veiculo))))
    (push novo-aluguel *alugueis*)
    (format t "Aluguel de veículo registrado com sucesso!.~%")
    novo-aluguel))

(defun calculo-valor-total (aluguel valor-diaria)
  (let ((dias-alugados (- (date-value (aluguel-dataDevolucao aluguel)) (date-value (aluguel-dataSaida aluguel))))
        (valor-total (* dias-alugados valor-diaria)))
    (setf (aluguel-valorTotal aluguel) valor-total)
    valor-total))

(defun listar-alugueis ()
  (if (null *alugueis*)
      (format t "Não há aluguéis cadastrados.~%")
      (dolist (aluguel *alugueis*)
        (format t "Cliente: ~a~%Data de Saída: ~a~%Data de Devolução: ~a~%Veículo: ~a~%Valor Total: R$ ~a~%~%"
                (buscar-cliente-por-cpf (aluguel-cpfCliente aluguel))
                (aluguel-dataSaida aluguel)
                (aluguel-dataDevolucao aluguel)
                (aluguel-veiculo aluguel)
                (aluguel-valorTotal aluguel)))))


(defun buscar-alugueis-por-cliente (cpf)
  (remove-if-not #'(lambda (aluguel)
                     (string= cpf (aluguel-cpfCliente aluguel)))
                 *alugueis*))

(defun listar-alugueis-por-cliente ()
  (let ((cpf (read-from-minibuffer "Informe o CPF do cliente: ")))
    (let ((alugueis (buscar-alugueis-por-cliente cpf)))
      (if (null alugueis)
          (format t "Não foram encontrados aluguéis para este cliente.~%")
          (dolist (aluguel alugueis)
            (format t "Data de Saída: ~a~%Data de Devolução: ~a~%Veículo: ~a~%Valor Total: R$ ~a~%~%"
                    (aluguel-dataSaida aluguel)
                    (aluguel-dataDevolucao aluguel)
                    (aluguel-veiculo aluguel)
                    (aluguel-valorTotal aluguel)))))))
