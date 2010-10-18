package test

import scala.xml.NodeSeq
import com.dridus.piper.web.Paths
import com.dridus.piper.web.basic.{notFound, sendXML}
import com.dridus.piper.web.core.{===>, Request, Response}
import com.dridus.piper.web.core.Pipeline.pipelineOps
import com.dridus.piper.web.servlet.Filter

import Paths.root

class Application extends Filter {
    private def surround(body: NodeSeq): NodeSeq =
        <html>
            <head><title>Hello World!</title></head>
            <body>{ body }</body>
        </html>

    val handle = notFound(sendXML(<h1>404!</h1>)) ===> Paths(
        root / {
            case "page1" => sendXML(<div>page 1!</div>)
            case "page2" => sendXML(<div>page 2!</div>)
        },

        root(sendXML(surround(<div><h1>Hello, World!</h1>Hey! <a href="page1">page 1</a> <a href="page2">page 2</a></div>)))
    )
}
