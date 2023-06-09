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
