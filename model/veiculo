(defstruct veiculo
  marca
  modelo
  inventario
  anoFabricacao
  anoModelo
  placa)

(defvar *veiculos* nil)

(defun cadastrar-veiculo()
  (let ((veiculo (make-veiculo :marca (read-from-minibuffer "Marca: ")
                               :modelo (read-from-minibuffer "Modelo: ")
                               :anoFabricacao (read-from-minibuffer "Ano Fabricação: ")
                               :anoModelo (read-from-minibuffer "Ano Modelo: ")
                               :placa (read-from-minibuffer "Placa: "))))
    (push veiculo *veiculos*)
    (format t "Veículo cadastrado com sucesso.~%")))

(defun buscar-veiculo-por-placa (placa)
  (find-if #'(lambda (veiculo)
               (string= placa (veiculo-placa veiculo)))
           *veiculos*))

(defun alterar-veiculo()
  (let ((placa (read-from-minibuffer "Informe a Placa do veículo a ser alterado: ")))
    (let ((veiculo (buscar-veiculo-por-placa placa)))
      (if veiculo
          (progn
            (setf (veiculo-marca veiculo) (read-from-minibuffer "Nova Marca: "))
            (setf (veiculo-modelo veiculo) (read-from-minibuffer "Novo Modelo: "))
            (setf (veiculo-anoFabricacao veiculo) (read-from-minibuffer "Novo Ano Fabricação: "))
            (setf (veiculo-anoModelo veiculo) (read-from-minibuffer "Novo Ano Modelo: "))
            (format t "Veículo alterado com sucesso!~%"))
          (format t "Veículo com Placa ~a não encontrado.~%" placa)))))

(defun excluir-veiculo()
  (let ((placa (read-from-minibuffer "Informe a placa do veículo a ser excluído: ")))
    (let ((veiculo (buscar-veiculo-por-placa placa)))
      (if veiculo
          (progn
            (setf *veiculos* (delete veiculo *veiculos*))
            (format t "Veículo excluído com sucesso!.~%"))
        (format t "Veículo com placa ~a não encontrado!.~%" placa)))))


(defun listar-veiculos()
  (if (null *veiculos*)
      (format t "Não há veículos cadastrados.~%")
      (dolist (veiculo *veiculos*)
        (format t "Marca: ~a%Modelo: ~a%AnoFabricação: ~a%AnoModelo: ~a%Placa: ~a~%~%"
                (veiculo-marca veiculo)
                (veiculo-modelo veiculo)
                (veiculo-anoFabricacao veiculo)
                (veiculo-anoModelo veiculo)
                (veiculo-placa veiculo)))))

