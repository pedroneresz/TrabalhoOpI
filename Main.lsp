(in-packge :user)

;; Controle de clientes ----------------------------------------------------------------------------------------------
(defstruct cliente
nome
cpf
endereco
telefone)

(defvar *clientes* nil)

(defun cadastrar-cliente(nome cpf endereco telefone)

    (let ((novo-cliente (make-cliente :nome nome
                                      :cpf cpf
                                      :endereco endereco
                                      :telefone telefone)))
        (push cliente *clientes*)
        (format t "Cliente cadastrado com Sucesso!")))

(defun buscar-cliente-por-cpf (cpf)
    (find-if #'(lambda (cliente)
        (string= cpf (cliente-cpf cliente)))
        *clientes*))

(defun alterar-cliente(nome cpf endereco telefone)
    (let ((cpf (buscar-cliene-por-cpf cpf)))
            (if cliente
                (progn
                    (listar-cliente cliente)
                (setf (cliente-nome cliente) nome
                      (cliente-cpf cliente) cpf
                      (cliente-endereco cliente) endereco
                      (cliente-telefone cliente) telefone)
                 (format t "Cliente alterado com sucesso!~%"))
                 (format t "Cliente com CPF  ~a não encontrado.~%" cpf))))

(defun excluir-cliente(cpf)
        (let ((cliente (buscar-cliente-por-cpf cpf)))
            (if cliente
              (progn
                (listar-cliente)
                  (if (yes-or-no-p (format nil "Confirma Exclusão?"))
                    (setf *clientes* (delete cliente *clientes* :test #'equal))
                        (format t "Cliente excluído com Sucesso!~%"))
                            (format t "Cliente com CPF ~a não encontrado.~%" cpf))))

(defun imprimir-clientes()
    if(nil *clientes*)
        (format t "Não há clientes cadastrados!.~%")
        (dolist (cliente *clientes*)
        (listar-cliente cliente)
        (format t "------------------------------------")))

(defun listar-cliente (cliente)
            (format t "Nome: ~a~%CPF:  ~a~%CNPJ: ~a~%Endereco: ~a~%Telefone: ~a~%"
          (cliente-nome cliente)
                (cliente-cfp cliente)
                (cliente-cnpj cliente)
                (cliente-endereco cliente)
                (cliente-telefone cliente)))

;; Controle de veiculos -----------------------------------------------------------------------------------------------
(defstruct veiculo
marca
modelo
inventario
anoFabricacao
anoModelo
placa)

(defvar *veiculos* nil)
;; Função para cadastro de veiculos
(defun cadastro-veiculo (marca modelo anoFabricacao anoModelo placa)
    (let ((veiculo (make-veiculo :marca marca
                                 :modelo modelo
                                 :anoFabricacao anoFabricacao
                                 :anoModelo anoModelo
                                 :placa placa )))
        (push veiculo *veiculos*)
        (format t "Veiculo cadastrado com sucesso")))

;; Função para busca de veiculos por placa
(defun buscar-veiculo-por-placa (placa)
    (find-if #'(lambda (veiculo)
        (string= placa (veiculo-placa veiculo)))
        *veiculos*))


;; Função para alteração de veiculos
(defun alterar-veiculo()
    (let ((placa ()))
        (let ((veiculo (buscar-veiculo-por-placa placa)))
            (if veiculo
                (progn
                (setf (veiculo-marca veiculo) Marca
                      (veiculo-modelo modelo) modelo
                      (veiculo-anoFabricacao) anoFabricacao
                      (veiculo-anoModelo) anoModelo
                      (veiculo-placa) placa
                 (format t "veiculo alterado com sucesso!~%"))
                 (format t "veiculo com Placa ~a não encontrado.~%" placa))))))

(defun atualizar-funcionario (codigo nome cpf cargo salario)
  (let ((funcionario (buscar-funcionario-por-cpf codigo)))
    (if funcionario
        (progn
          (imprimir-funcionario funcionario)
          (if (yes-or-no-p "Deseja atualizar esses dados? ")
              (progn
                (setf (funcionario-nome funcionario) nome
                      (funcionario-cpf funcionario) cpf
                      (funcionario-cargo funcionario) cargo
                      (funcionario-salario funcionario) salario)
                (format t "Funcionário atualizado com sucesso.~%"))
              (format t "Operação cancelada.~%")))
        (format t "Funcionário não encontrado.~%"))))


;; Função para exclusão de veiculos
(defun excluir-veiculo()
    (let ((placa (read-from-minibuffer "Informe a placa do carro a ser excluído: "))
        (let ((veiculo (buscar-veiculo-por-placa)))
            (if(veiculo
                (listar-veiculo veiculo)
                    (progn
                        (setf *veiculos* (delete veiculo *veiculos*))
                        (format t "Veiculo excluído com sucesso!.~%"))
                        (format t "Veiculo com placa ~a Não encontrado!.~%" placa)))))))

(defun listar-veiculo (veiculo)
    (format t "Marca: ~a~%Modelo: ~a~%AnoFabricação: ~a~%AnoModelo: ~a~%Placa: ~a~%"
          (veiculo-marca veiculo)
                    (veiculo-modelo veiculo)
                    (veiculo-anoFabricacao veiculo)
                    (veiculo-anoModelo veiculo)
                    (veiculo-placa veiculo)))

;; Função para imprimir todos os funcionários cadastrados
(defun imprimir-veiculos ()
  (dolist (veiculo *veiculos*)
    (listar-veiculo veiculo)
    (format t "------------------~%")))

;; Função para remover um funcionário
(defun remover-funcionario (codigo)
  (let ((funcionario (buscar-funcionario-por-codigo codigo)))
    (if funcionario
        (if (funcionario-flag funcionario)
            (format t "Impossível remover, funcionário vinculado a uma OS.")
          (progn
            (imprimir-funcionario funcionario)
            (if (yes-or-no-p (format nil "Deseja excluir o funcionário ?"))
                (setq *funcionarios* (remove funcionario *funcionarios* :test #'equal))
                (format t "Exclusão cancelada."))))
        (format t "Funcionário não encontrado."))))

;; Controle de Alugueis -------------------------------------------------------------------------------------

(defstruct aluguel
dataSaida
dataDevolucao
status
cliente
veiculo
valorTotal
diasAlugados
)

(defvar *alugueis* nil)
(defvar *valor-diaria* 150)

(defun cadastrar-aluguel (cpfCliente placaVeiculo dataSaida dataDevolucao)
(let ((novo-aluguel (make-aluguel :dataSaida dataSaida
                                  :dataDevolucao dataDevolucao
                                  :cliente (buscar-cliente-por-cpf cpfCliente)
                                  :veiculo (buscar-veiculo-por-placa placaVeiculo))
            (push aluguel *alugueis*)
            (format t "Aluguel de veiculo registrado com sucesso!.~%")
            aluguel))))

(defun calculo-valor-valorTotal (aluguel valorDiaria)
    (let ((diasAlugados (-(date-value (aluguel-dataDevolucao aluguel)) (date-value (aluguel-dataSaida))))
        (valorTotal (* dias-alugados valorDiaria)))
            (setf (aluguel-dia-valor-total aluguel)valorTotal)
            valorTotal)) 

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

;; Controla o pagamento dos alugueis ------------------------------------------------------------------

(defstruct pagamento
  cpfCliente      
  placaVeiculo    
  formaPagamento  ;(cartão ou dinheiro)
  metodoPagamento ; (à vista ou parcelado)
  parcelas        
  status  )        ; (aberto ou pago)

  ;Registra um novo pagamento na base de dados.
(defun cadastrar-pagamento (cpf-Cliente placa-Veiculo forma-Pagamento metodo-Pagamento valor-total num-parcelas &optional (juros 0))
   
  (let ((novo-pagamento (make-pagamento :cpfCliente cpf-Cliente
                                        :placaVeiculo placa-Veiculo
                                        :formaPagamento forma-Pagamento
                                        :metodoPagamento metodo-Pagamento
                                        :status "aberto")))
    (when (= num-parcelas 1)
      (setf (pagamento-status novo-pagamento) "pago à vista")
      (setf (pagamento-parcelas novo-pagamento) nil)
      (setf (car (pagamento-parcelas novo-pagamento)) (make-parcela :numero 1 :valor valor-total :juros juros))
      (format t "Pagamento à vista de R$~a efetuado com sucesso.~%" valor-total)
      (return-from cadastrar-pagamento novo-pagamento))
    (let ((valor-parcela (/ (+ (* valor-total (+ juros 1)) 0.5) num-parcelas)))
      (setf (pagamento-parcelas novo-pagamento)
            (loop for i from 1 to num-parcelas
                  collect (make-parcela :numero i :valor valor-parcela :juros juros))))
    (format t "Pagamento parcelado em ~a vezes no valor total de R$~a efetuado com sucesso.~%"
            num-parcelas (* num-parcelas valor-parcela))
    novo-pagamento))

(defun buscar-pagamento (cpf placa)
  ;Busca um pagamento na base de dados a partir do CPF do cliente e da placa do veículo.
  
  (find-if #'(lambda (pagamento)
               (and (string= cpf (pagamento-cpfCliente pagamento))
                    (string= placa (pagamento-placaVeiculo pagamento))))
           *pagamentos*))

(defun atualizar-pagamento (cpf placa status)
  ;Atualiza o status de um pagamento na base de dados a partir do CPF do cliente, da placa do veículo 
   ;e do novo status a ser atribuído.Retorna T se a atualização foi bem-sucedida, ou NIL caso contrário.
  (let ((pagamento (buscar-pagamento cpf placa)))
    (when pagamento
      (setf (pagamento-status pagamento) status)
      t)))

  ;;Exclui um pagamento da base de dados a partir do CPF do cliente e da placa do veículo.
  ;; Retorna T se a exclusão foi bem-sucedida, ou NIL caso contrário.
(defun excluir-pagamento (cpf placa)
  (let ((pagamento (buscar-pagamento cpf placa)))
    (when pagamento
      (setq *pagamentos* (remove pagamento *pagamentos*))
      t)))

 ;; "Lista todos os pagamentos cadastrados na base de dados."
(defun listar-pagamentos ()
  (if (null *pagamentos*)
      (format t "Não há pagamentos cadastrados.~%")
      (dolist (pagamento *pagamentos*)
        (format t "CPF do cliente: ~a~%Placa do veículo: ~a~%Forma de pagamento: ~a~%Método de pagamento: ~a~%Status"
              (pagamento-cpf-cliente pagamento)
              (pagamento-placa-veiculo pagamento)
              (pagamento-forma-pagamento pagamento)
              (pagamento-metodo-pagamento pagamento)
              (pagamento-status pagamento)))))
