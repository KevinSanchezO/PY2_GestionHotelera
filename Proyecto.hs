import System.IO
import Control.DeepSeq
import Control.Exception
import Text.ParserCombinators.Parsec.Language (GenLanguageDef(reservedOpNames))
import Data.Array (listArray)
import GHC.ForeignPtr (finalizeForeignPtr)
import System.Console.Haskeline (Interrupt(Interrupt))

menu::IO()
menu=
    do
        putStrLn "\nBienvenido al menu principal\n 1. Opciones Administrativas\n 2. Opciones Generales\n 0.Salir\n Digite una opcion: "
        opcion<-getLine
        case opcion of
            "1" -> menuAdministrativo
            "2"->menuOpcionesGenerales
            "0"->return()
            _ -> errorDigito



menuAdministrativo :: IO()
menuAdministrativo= 
    do 
        putStrLn "\n*****Opciones Administrativas*****\n 1.Informacion de hotel\n 2.Cargar tipo de habitaciones\n 3.Asignar cantidad de habitaciones por tipo\n 4.Carga de tarifas\n 5.Consultar reservacion\n 6.Consulta de facturas\n 7.Estadisticas de ocupacion\n Digite una opcion: "
        s<-getLine
        case s of
            "1" ->infoHoteles
            "2"->cargarTipoHabitacion
            "3"->asignarCantidadHabitacionesTipo
            {-"4"->cargarTarifas
            "5"->consultarReservacion
            "6"->consultaFacturas
            "7"->estadisticasOcupacion-}
            "0"->menu
            _ -> errorDigito



menuOpcionesGenerales:: IO()
menuOpcionesGenerales=
    do
        putStrLn "\n******Opciones Generales*****\n 1.Reservacion\n 2.Cancelar Reservacion\n 3.Facturar Reservacion\n 4.Salir\n Digite una opcion: "
        d<-getLine
        case d of
            {-"1"->reservacion
            "2"->cancelarReservacion
            "3"->facturarReservacion
            "4"->menu-}
            _ ->errorDigito



errorDigito::IO()
errorDigito=
    do
        putStr "\ESC[2J"
        putStrLn "Error!!, Digite una opcion valida"
        menu

{--------------------------------------------------------------------------------------------------------------------------}


infoHoteles:: IO()
infoHoteles=
    do
        putStrLn "\nInformacion del hotel\nNombre   Cedula Juridica   Sitio Web   Telefono   Pais   Provincia"
        cs <- readFile "infoHoteles.txt"
        putStrLn cs


{--------------------------------------------------------------------------------------------------------------------------}

leerArchivo :: System.IO.FilePath -> IO [String]
leerArchivo (archivo) = do
    contents <- readFile archivo
    evaluate (force contents)
    let lineas = lines contents
    return lineas;


escribirEnArchivo :: (System.IO.FilePath, [Char]) -> IO ()
escribirEnArchivo (archivo, algo) = do
    appendFile archivo (algo ++ "\n")


reEscribir :: System.IO.FilePath -> String -> IO ()
reEscribir archivo info = do
    writeFile archivo (info)


estaLista :: String->[String]->Bool
estaLista nombre [] = False
estaLista nombre lista = do
    let primerElemento = head lista
    let listaCortada = tail lista
    if primerElemento == nombre then
        True
    else
        estaLista nombre listaCortada



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



escribirEnHabitaciones :: System.IO.FilePath->[String]->IO ()
escribirEnHabitaciones archivo [] = menuAdministrativo
escribirEnHabitaciones archivo lista = do
    let hd = head lista
    let tl = tail lista
    appendFile archivo (hd ++ "\n")
    escribirEnHabitaciones archivo tl



cargarTipoHabitacion:: IO()
cargarTipoHabitacion =
    do 
        putStrLn "\nIngrese la ruta del archivo con los tipos de habitaciones: "
        ruta <- getLine
        listaA <- leerArchivo "habitaciones.txt"
        listaB <- leerArchivo ruta
        
        let listaC = listaNoRepetidos listaB listaA 0
        reEscribir "habitaciones.txt" ""
        escribirEnHabitaciones "habitaciones.txt" listaC
        menuAdministrativo
       

{--------------------------------------------------------------------------------------------------------------------------}

asignarCantidadHabitacionesTipo:: IO()
asignarCantidadHabitacionesTipo =
    do
        listaA <- leerArchivo "habitaciones.txt"
        let listaB = listaHabitaciones listaA []
        reEscribir "cantidadesTipos.txt" ""
        reEscribir "codigosTipos.txt" ""
        cargarCantidades "cantidadesTipos.txt" listaB


StringAEntero texto =
    do
        let valor = read texto::Integer
        return valor
        

cargarCantidades:: System.IO.FilePath -> [String]->IO()
cargarCantidades archivo [] = mostrarCantidades
cargarCantidades archivo lista =
    do
        let cabeza = head lista
        let final = tail lista

        putStrLn "Digite la cantidad de habitaciones para el tipo "++ cabeza ++ ": "
        cantidad <-getLine 
        appendFile archivo (cabeza ++ "\n" ++ cantidad ++ "\n")
        cantidadAux <- StringAEntero(cantidad)
        generarTiposHabitaciones archivo cabeza cantidadAux 0 final "codigoTipos.txt"


generarTiposHabitaciones:: System.IO.FilePath -> String ->Integer->Integer[String]->System.IO.FilePath->IO()
generarTiposHabitaciones archivo nombreTipo cantidad temp listaTipos archivoAux =
    do
        let cont = cont+1
        contAux <- EnteroAString temp
        let string = nombreTipo ++ contAux ++"\n"
        if temp < cantidad then
            guardarCantidad archivo nombreTipo cantidad cont listaTipos string archivoAux
        else
            cargarCantidades archivo listaTipos

guardarCantidad::System.IO.FilePath -> String -> Integer -> Integer -> [String] -> String -> System.IO.FilePath -> IO()
guardarCantidad archivo nombre cantidad temp listaTipos string archivoAux =
    do
        appendFile archivoAux string
        generarTipoHabitaciones archivo nombre cantidad temp listaTipos archivoAux


listaHabitaciones::[String]->[String]->[String]
listaHabitaciones [] res = res
listaHabitaciones textoLista res =
    do
        let cabeza = head textoLista
        let final = tail textoLista
        let final1 = tail final
        let final2 = tail final
        let resultado = res++[cabeza]
        listaHabitaciones final2 resultado

mostrarCantidades::IO()
mostrarCantidades =
    do
        listaA <- leerArchivo("codigosTipos.txt")
        listaB <- leerArchivo("cantidadesTipos.txt")
        printearCantidades listaB listaA 

printearCantidades::[String]->[String]->IO()
printearCantidades [] [] = menuAdministrativo
printearCantidades listaA listaB = do
    let nombre = head listaA
    let final1 = tail listaA
    let final2 = tail final1
    let cantidad = read(head final2)::Int
    putStrLn "Tipo: "++nombre++", IDs: "
    printIdentificadores final2 cantidad 0 listaB

printIdentificadores::[String]->Int->Int->[String]->IO()
printIdentificadores listaA cantidad cont listaB =
    do 
        let cabeza = head listaB         
        let final = tail listaB
        let temp = cont+1
        putStrLn (cabeza++"\n")
        if (cont == cantidad-1) then
            printearCantidades listaA final
        else
            printIdentificadores listaA cantidad temp listaB
