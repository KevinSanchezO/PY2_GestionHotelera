import System.IO
import Control.DeepSeq
import Control.Exception



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
            {-"3"->asignarCantidadHabitacionesTipo
            "4"->cargarTarifas
            "5"->consultarReservacion
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
        putStrLn "\nInformacion del hotel\nNombre\t\tCedula Juridica\t\tSitio Web\tTelefono\tPais\tProvincia"
        cs <- readFile "infoHoteles.txt"
        let datos = split ',' cs
        mostrarAux datos

{----mostrarAux----
Menu para mostrar de forma tabulada los datos del archivo--}
mostrarAux::[String]->IO()
mostrarAux [] = menuAdministrativo
mostrarAux datosArchivo = 
    do
        let cabeza = head datosArchivo
        let resto = tail datosArchivo
        let stringMensaje = cabeza++"\t"
        putStr stringMensaje
        mostrarAux resto

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
leerArchivo (archivo) = do
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
listaNoRepetidos list res opc = do
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
resetearArchivo archivo info = do
    writeFile archivo (info)

{----estaLista----
verifica si en una lista se encuentra un valor en especifico-}
estaLista :: String->[String]->Bool
estaLista nombre [] = False
estaLista nombre lista = do
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
escribirEnHabitaciones archivo lista = do
    let dato = head lista
    let tl_1 = tail lista

    let guardarMensaje = dato  ++ "\n"
    appendFile archivo guardarMensaje
    escribirEnHabitaciones archivo tl_1
       
{--------------------------------------------------------------------------------------------------------------------------}

stringToInt texto = do
        let valor = read texto::Integer
        return valor
        

intToString numero = do
        let valor = show numero
        return valor


