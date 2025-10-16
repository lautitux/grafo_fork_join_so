export const EXAMPLES = {
    "Simple 1": "-- Esto es un comentario\nA\nFORK L1\nB\nQUIT\nL1: C",
    "Simple 2": "-- No importa la capitalización de nada\n-- todo se considera como si estuviese en mayúsculas\nL1: T1\nFork L3\nL2: T2\nQuit\nL3: T3\nT4",
    "Join 1": "-- Al declarar un contador no debe haber espacios\n-- separando el nombre y/o el número del signo de igualdad (=)\nContador=2\nA\nFORK LC\nB\nGOTO JD\nLC: C\nJD: JOIN Contador, LD\nQUIT\nLD: D",
    "Join 2": "Contador=2\nFORK LD\nA\nFORK JC\nB\nQUIT\nJC: JOIN Contador, LC\nQUIT\nLC:\nC\nQUIT\nLD:\nD\nGOTO JC",
    "Complejo": "ContF=2\nContG=3\nFORK LA\nFORK LD\nH\nFORK LJF\nGOTO LJG\nLD: D\nFORK LE\nGOTO LJF\nQUIT\nLE: E\nGOTO LJG\nLA: A\nFORK LC\nB\nQUIT\nLC: C\nLJG: JOIN ContG, LG\nQUIT\nLG: G\nQUIT\nLJF: JOIN ContF, LF\nQUIT\nLF: F"
}

export function load_examples() {
    let select = document.getElementById("select-ejemplo")
    for (const key of Object.keys(EXAMPLES)) {
        let option = document.createElement("option")
        option.value = key
        option.appendChild(document.createTextNode(key))
        select.appendChild(option)
    }
}