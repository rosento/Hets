import * as React from "react"
import * as ReactDOM from "react-dom"

import { OpenLogicButton } from "./components/OpenLogic"

ReactDOM.render(
    <OpenLogicButton onClick={ () => alert("foobar") } />,
    document.getElementById("example")
);