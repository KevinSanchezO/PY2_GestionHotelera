import System.IO
import Control.DeepSeq
import Control.Exception
{-import Text.ParserCombinators.Parsec.Language (GenLanguageDef(reservedOpNames, reservedNames))
import Data.Array (listArray)
import GHC.ForeignPtr (finalizeForeignPtr)
import System.Console.Haskeline (Interrupt(Interrupt))
import GHC.Real (numerator)
import Distribution.Simple.InstallDirs (defaultInstallDirs)-}


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
            {-"2"->cargarTipoHabitacion
            "3"->asignarCantidadHabitacionesTipo
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

