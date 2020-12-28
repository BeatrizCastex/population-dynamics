	! Programa que utiliza o método RK4 para integras as equações de Lotka-Volterra para a interação entre predadores e presas

	program presa_predador
	IMPLICIT NONE

	!Variáveis
	real*8 :: presa 		! Número de presas na geração atual
	real*8 :: predador 		! Número de predadores na geração atual
	real*8, dimension(4) :: Fps 	! Aproximação de taxa de crescimento para presas
	real*8, dimension(4) :: Fpd 	! Aproximação de taxa de crescimento para predadores
	real*8, dimension(4) :: AJUps 	! Número de presas ajustado para aproximação
	real*8, dimension(4) :: AJUpd 	! Número de presas ajustado para aproximação
	real*8, dimension(4) :: C 	! Constantes de integração
	real*8 :: t 			! Tempo decorrido
	real*8 :: t_max 		! Tempo máximo decorrido
	real*8 :: dt 			! Variação do tempo
	integer*8 :: j 			! Contador
	real*8, external :: f 		! Função de evolução da população da presa
	real*8, external :: g 		! Função de evolução da população do predador

	! Abrindo arquivos para imprimir variáveis
	open(1, file = "presas_vs_tempo.dat")
	open(2, file = "predadores_vs_tempo.dat")
	open(3, file = "presas_vs_predadores.dat")


	! Inicializando variáveis	
	presa = 2
	predador = 1
	t = 0.d0
	t_max = 100.d0
	dt = 0.001d0
	C(2) = 0.5d0
	C(3) = 0.5d0
	C(4) = 1.d0

	DO WHILE (t < t_max)
	
		! Calculando as aproximações de taxa de variação iniciais
		Fps(1) = f(presa,predador)
		Fpd(1) = g(presa,predador)
		
		! Calculando os outros ajustes e aproximações de taxa de variação
		DO j = 2,4

			AJUps(j) = presa + ( dt * C(j) * Fps(j-1) )
			AJUpd(j) = predador + ( dt * C(j) * Fpd(j-1) )

			Fps(j) = f(AJUps(j),AJUpd(j))
			Fpd(j) = g(AJUps(j), AJUpd(j))

		enddo

		! Calculando populações em determinado tempo
		presa = presa + ( ( dt / 6.d0) * ( Fps(1) + ( 2 * ( Fps(2) + Fps(3) ) ) + Fps(4) ) )
		predador = predador + ( ( dt / 6.d0) * ( Fpd(1) + ( 2 * ( Fpd(2) + Fpd(3) ) ) + Fpd(4) ) )

		! Calulando tempo
		t = t + dt

		! Imprimindo resultados
		write(1,*)t, presa
		write(2,*)t,predador
		write(3,*)presa,predador

		
	enddo


	end program presa_predador

	real*8 function f(x,y)
	IMPLICIT NONE
	
	!Variáveis:
	real*8, intent(in)  :: x !Variável de entrada das presas
	real*8, intent(in)  :: y !Variável de entrada dos predadores
	real*8 :: a !Taxa de crescimento das presas
	real*8 :: b ! Taxa de crescimento dos predadores em relação as presas

	a = 2.d0/3.d0
	b = 4.d0/3.d0

	f = ( a * x ) - (b * x * y )
	
	RETURN 
	end

	real*8 function g(x,y)
	IMPLICIT NONE
	
	!Variáveis:
	real*8, intent(in)  :: x !Variável de entrada das presas
	real*8, intent(in)  :: y !Variável de entrada dos predadores
	real*8 :: c !Taxa de crescimento dos pedadores
	real*8 :: d ! Taxa de crescimento das presas em relação aos predadores

	c = 1.d0
	d = 1.d0

	g = - ( c * y ) + (d * x * y )
	
	RETURN 
	end
