import Control.Monad
import Data.Char
import System.IO
import Network
import Data.Time.LocalTime

data RequestType = GET | POST deriving (Show)
data Request = Request { rtype :: RequestType, path :: String } deriving (Show)
data Response = Response { version :: String, statuscode :: Int }

instance Show Response where
	show r = version(r) ++ " " ++ show(statuscode(r)) ++ " " ++ (case statuscode(r) of
		100 -> "Continue"
		200 -> "OK"
		404 -> "Not Found") ++ "\r\n\r\n"

respond :: Request -> Handle -> IO ()
respond request handle = do
	let response = Response {version = "HTTP/1.1", statuscode = 200}
	hPutStr handle $ show(response)
	time <- getZonedTime
	hPutStr handle $ "Haskell says HELLO.\nThe time is currently " ++ show(time)

--- expects something like "GET / HTTP/1.1"
createHeadRequest :: String -> Request
createHeadRequest line = case words(line) of
	["GET",p,_] -> Request {rtype = GET, path=p}
	["POST",p,_] -> Request {rtype = POST, path=p}

parseRequest :: [String] -> Request
parseRequest received = createHeadRequest(head(received))

handleAccept :: Handle -> String -> IO ()
handleAccept handle hostname = do 
	putStrLn $ "Handling request from " ++ hostname
	recvd <- hGetContents handle
	let request = parseRequest(lines(recvd))
	respond request handle
	return ()
	
main = withSocketsDo $ do
	sock <- listenOn (PortNumber 9000)
	putStrLn "Listening on port 9000"
	forever $ do
		(handle, hostname, port) <- accept sock
		handleAccept handle hostname
		hClose handle
	