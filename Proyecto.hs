import System.IO
import Control.DeepSeq ( force )
import Control.Exception ( evaluate )


{-----Menu de inicio----
Menu el cual se mostrara primero al iniciar el programa-}
menuPrincipal::IO()
menuPrincipal=
    do
        putStrLn "\nBienvenido al menu principal\n 1. Opciones Administrativas\n 2. Opciones Generales\n 0.Salir\n Digite una opcion: "
        opcion<-getLine
        case opcion of
            "1" -> menuAdministrativo
            "2"->menuOpcionesGenerales
            "0"->return()
            _ -> errorDigito


{-----Menu de opciones administrativas----
Menu donde se permite acceder a las opciones administrativas del sistema-}
menuAdministrativo :: IO()
menuAdministrativo= 
    do 
        putStrLn "\n\n*****Opciones Administrativas*****\n 1.Informacion de hotel\n 2.Cargar tipo de habitaciones\n 3.Asignar cantidad de habitaciones por tipo\n 4.Carga de tarifas\n 5.Consultar reservacion\n 6.Consulta de facturas\n 7.Estadisticas de ocupacion\n 0.Volver\n Digite una opcion: "
        s<-getLine
        case s of
            "1" ->infoHoteles
            "2"->cargarTipoHabitacion
            "3"->asignarCantidadHabitacionesPorTipo
            "4"->cargarTarifas
            {-"5"->consultarReservacion
            "6"->consultaFacturas
            "7"->estadisticasOcupacion-}
            "0"->menuPrincipal
            _ -> errorDigito


{-----Menu de opciones generales----
Menu donde se permite acceder a las opciones generales del sistema-}
menuOpcionesGenerales:: IO()
menuOpcionesGenerales=
    do
        putStrLn "\n\n******Opciones Generales*****\n 1.Reservacion\n 2.Cancelar Reservacion\n 3.Facturar Reservacion\n 0.Salir\n Digite una opcion: "
        d<-getLine
        case d of
            {-"1"->reservacion
            "2"->cancelarReservacion
            "3"->facturarReservacion-}
            "0"->menuPrincipal
            _ ->errorDigito


{-----errorDigito----
Menu el cual se mostrara primero al iniciar el programa-}
errorDigito::IO()
errorDigito=
    do
        putStr "\ESC[2J"
        putStrLn "\nERROR, Digite una opcion valida\n"
        menuPrincipal

{-------------------------------------------------------INFORMACION DE HOTELES-------------------------------------------------------------------}

{----infoHoteles----
Muestra los datos del hotel administrativo leyendo el archivo que almacena los datos--}
infoHoteles:: IO()
infoHoteles=
    do
        putStrLn "\n_____________________INFORMACION DEL HOTEL_____________________"
        cs <- readFile "infoHoteles.txt"
        let datos = split ',' cs
        mostrarAux datos

{----mostrarAux----
Menu para mostrar de forma tabulada los datos del archivo--}
mostrarAux::[String]->IO()
mostrarAux [] = menuAdministrativo
mostrarAux datosArchivo = 
    do
        let nombre = head datosArchivo
        let mensajeNombre = "Nombre: "++nombre
        let resto1 = tail datosArchivo

        let cedula = head resto1
        let mensajeCedula = "Cedula Juridica: "++cedula 
        let resto2 = tail resto1 

        let sitioWeb = head resto2
        let mensajeSitioWeb = "Sitio Web: "++sitioWeb
        let resto3 = tail resto2

        let telefono = head resto3
        let mensajeTelefono = "Telefono: "++telefono
        let resto4 = tail resto3

        let pais = head resto4
        let mensajePais = "Pais: "++pais
        let resto5 = tail resto4

        let provincia = head resto5
        let mensajeProvincia = "Provincia: "++provincia

        putStrLn mensajeNombre
        putStrLn mensajeCedula
        putStrLn mensajeSitioWeb
        putStrLn mensajeTelefono
        putStrLn mensajePais
        putStrLn mensajeProvincia
        menuAdministrativo

{----split----
Separa en listas un string con un valor determinado tipo char--}
split :: Char -> String -> [String]
split c xs = case break (==c) xs of 
    (ls, "") -> [ls]
    (ls, x:rs) -> ls : split c rs

{-------------------------------------------------------CARGA DE TIPO DE HABITACION-------------------------------------------------------------------}

{----cargarTipoHabitaciones----
solicita al usuario la ruta del archivo con los datos de habitaciones que desea ingresar
y administra el proceso de guardar dichos datos-}
cargarTipoHabitacion::IO()
cargarTipoHabitacion =
    do 
        putStrLn "\nIngrese la ruta del archivo con los tipos de habitaciones: "
        ruta <- getLine
        listaA <- leerArchivo "habitacionesCargadas.txt"

        contenido <- readFile ruta
        let listaB = split ',' contenido
         
        let listaC = listaNoRepetidos listaB listaA 0
        resetearArchivo "habitacionesCargadas.txt" ""
        escribirEnHabitaciones "habitacionesCargadas.txt" listaC
        menuAdministrativo
        
{----leerArchivo----
Permite cargar en una lista los datos de un archivo cuyos datos esten separados por
saltos de linea haciendo cada salto de linea un dato dentro de la lista-}
leerArchivo :: System.IO.FilePath -> IO [String]
leerArchivo (archivo) = 
    do
        contents <- readFile archivo
        evaluate (force contents)
        let lineas = lines contents
        return lineas;

{----listaNoRepetidos----
verifica que en el archivo donde persisten los datos de las habitaciones
no existan nombre de habitaciones repetidos
-}
listaNoRepetidos :: [String] -> [String] -> Int ->[String]
listaNoRepetidos [] res opc = res
listaNoRepetidos list res opc = 
    do
        let listaCortada = tail list
        let listaCortada1 = tail listaCortada
        let listaCortada2 = tail listaCortada1
        let listaCortada3 = tail listaCortada2
        let hd = head list
        let res2 = res ++ [hd]
        let opc1 = opc+1
        let opc2 = opc+3
        (if ((mod opc  3) == 0) && estaLista hd res then
             listaNoRepetidos listaCortada2 res opc2
        else
            listaNoRepetidos listaCortada res2 opc1)

{----resetearArchivo----
Permite dejar en blanco un archivo de texto para volver a escribir su informacion
modificada-}
resetearArchivo :: System.IO.FilePath -> String -> IO ()
resetearArchivo archivo info = 
    do
        writeFile archivo (info)

{----estaLista----
verifica si en una lista se encuentra un valor en especifico-}
estaLista :: String->[String]->Bool
estaLista nombre [] = False
estaLista nombre lista = 
    do
        let primerElemento = head lista
        let listaCortada = tail lista
        if primerElemento == nombre then
            True
        else
            estaLista nombre listaCortada


{----escribirEnHabitaciones----
se encarga de administrar la escritura de los datos de habitaciones sin que
se repita datos de habitaciones en el archivo de la persistencia de datos-}
escribirEnHabitaciones :: System.IO.FilePath->[String]->IO ()
escribirEnHabitaciones archivo [] = menuAdministrativo
escribirEnHabitaciones archivo lista = 
    do
        let dato = head lista
        let tl_1 = tail lista

        let guardarMensaje = dato  ++ "\n"
        appendFile archivo guardarMensaje
        escribirEnHabitaciones archivo tl_1
       
{--------------------------------------------------------------------------------------------------------------------------}

stringToInt texto = 
    do
        let valor = read texto::Integer
        return valor
        

intToString numero = 
    do
        let valor = show numero
        return valor

{------------------------------------------------CANTIDAD HABITACIONES POR TIPO---------------------------------------------}

{----asignarCantidadHabitacionesPorTipo----
Administra todos los archivos necesarios para poder hacer esta funcionalidad-}
asignarCantidadHabitacionesPorTipo:: IO()
asignarCantidadHabitacionesPorTipo = 
    do
        listaHabitacion <- leerArchivo"habitacionesCargadas.txt"
        let listaHab = crearListaHabitacionesAux listaHabitacion []
        resetearArchivo "cantidadPorTiposHabitaciones.txt" ""
        resetearArchivo "codigosTiposHabitaciones.txt" ""
        asignarCantidadesDeHabiatciones "cantidadPorTiposHabitaciones.txt" listaHab

{----asignarCantidadesDeHabiatciones----
Le solicita al usuario la cantidad de habitaciones que se crearan por tipo almacenado-}
asignarCantidadesDeHabiatciones:: System.IO.FilePath->[String]->IO()
asignarCantidadesDeHabiatciones archivo [] = mostrarCantidades
asignarCantidadesDeHabiatciones archivo listahab = 
    do
        let nombre = head listahab
        let tl = tail listahab
        let mensaje = "\tDigite cuantas habitaciones tendra el hotel de este tipo ->" ++ nombre ++ " : "
        putStrLn mensaje
        cantidad <- getLine
        appendFile archivo (nombre ++ "\n")
        appendFile archivo (cantidad ++ "\n")
        cantidad2 <- stringToInt(cantidad)
        agregarToHabitaciones archivo nombre cantidad2 0 tl "codigosTiposHabitaciones.txt"

{-----agregarToHabitaciones----
Genera el codigo unico de las habitaciones creadas-}
agregarToHabitaciones:: System.IO.FilePath->String->Integer->Integer->[String]->System.IO.FilePath->IO()
agregarToHabitaciones archivo nombre cantidad contador lista archivoCodigos = 
    do 
        let contador2 = contador + 1
        pContador2 <- intToString contador
        let catidadDefinitiva = nombre++pContador2++"\n"
        if contador < cantidad then
            escribirEnCantidadPorTipo archivo nombre cantidad contador2 lista catidadDefinitiva archivoCodigos
        else
            asignarCantidadesDeHabiatciones archivo lista

{-----escribirEnCantidadPorTipo-----
Agrega al archivo de la persistencia de datos el codigo de una habitacion creada.-}
escribirEnCantidadPorTipo::System.IO.FilePath->String->Integer->Integer->[String]->String->System.IO.FilePath->IO()
escribirEnCantidadPorTipo archivo nombre cant cont2 lista catidadDefinitiva archivo2=
    do 
        appendFile archivo2 catidadDefinitiva 
        agregarToHabitaciones archivo nombre cant cont2 lista archivo2
    
{----mostrarCantidades----
Se encarga de preparar los archivos para mostrar las cantidades de habitaciones creadas-}
mostrarCantidades::IO()
mostrarCantidades = 
    do
        listaCodigosTipoHabitacion <- leerArchivo "codigosTiposHabitaciones.txt"
        listaCantidadTiposHabitacion<- leerArchivo "cantidadPorTiposHabitaciones.txt"
        mostrarCantidadesAux listaCantidadTiposHabitacion listaCodigosTipoHabitacion

{----mostrarCantidadesAux----
Muestra el nombre y el tipo de habitacion para luego mostrar la cantidades de ese tipo de habitacion-}
mostrarCantidadesAux::[String]->[String]->IO()
mostrarCantidadesAux [] [] = menuAdministrativo
mostrarCantidadesAux listaCodigosTipoHabitacion listaCantidadTiposHabitacion = 
    do
        let nombre = head listaCodigosTipoHabitacion
        let tl2 = tail listaCodigosTipoHabitacion 
        let tl3 = tail tl2
        let cantidad = read(head tl2)::Int
        let mensaje = "El tipo de habitacion "++nombre++" tiene las siguientes habitaciones, se muestran sus identificadores: \n"
        putStrLn mensaje
        mostrarSig tl3 cantidad 0 listaCantidadTiposHabitacion


{----mostrarSig----
muestra la cantidad de habitaciones por tipo y continua la funcion recursiva 
de mostrar las cantidades de habitaciones por tipo-}
mostrarSig::[String]->Int->Int->[String]->IO()
mostrarSig lista cant cont listaCantidadTiposHabitacion = 
    do
        let hd = head listaCantidadTiposHabitacion
        let tl = tail listaCantidadTiposHabitacion
        let cont2 = cont+1
        putStrLn (hd++"\n")
        if cont == cant-1 then 
            mostrarCantidadesAux lista tl
        else
            mostrarSig lista cant cont2 tl

{-----------------------------------------------------------CARGAR TARIFAS---------------------------------------------------------------}

{----cargarTarifas-----
Se encarga de preparar los archivos necesarios para esta funcionalidad para
realizar la persistencia de datos-}
cargarTarifas::IO()
cargarTarifas = 
    do
        resetearArchivo "Tarifas.txt" ""
        list <- leerArchivo "habitacionesCargadas.txt"
        let listaDeHabitaciones = crearListaHabitacionesAux list []
        cargarTarifasAux listaDeHabitaciones
       
{----cargarTarifasAux----
Le solicita al usario la tarifa o precio de cada tipo de habitacion para
luego guardar cada tarifa en el archivo de persistencia de datos-}
cargarTarifasAux ::[String]->IO()
cargarTarifasAux [] = menuAdministrativo 
cargarTarifasAux lista = 
    do
        let tipo = head lista
        let tlLista = tail lista
        let interaccion = ("\nIngrese un precio para la habitacion de tipo " ++ tipo ++ " : ")
        putStrLn interaccion
        precio <- getLine
        appendFile "Tarifas.txt" (tipo ++ "\n" ++ precio ++ "\n")
        cargarTarifasAux tlLista     

{----crearListaHabitacionesAux----
crea una lista con los nombres de los tipos de habitaciones cargadas-}
crearListaHabitacionesAux:: [String]->[String]->[String]
crearListaHabitacionesAux [] listaDeHabitaciones = listaDeHabitaciones
crearListaHabitacionesAux lista listaDeHabitaciones = 
    do
        let nombre = head lista
        let valor_1 = tail lista
        let valor2 = tail valor_1
        let listaSigHabitacion = tail valor2
        let listaDeHabitaciones2 = listaDeHabitaciones++[nombre] 
        crearListaHabitacionesAux listaSigHabitacion listaDeHabitaciones2