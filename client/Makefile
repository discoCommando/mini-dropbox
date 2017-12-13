../assets/index.html: Api.elm Main.elm
	elm make --yes Main.elm --output index.js
	mv index.js ../assets/

Api.elm: GenerateElm.hs ../server/src/Api.hs
	(cd ../ ; stack exec -- runhaskell -iserver/src client/GenerateElm.hs; cd client/ ; elm-format --yes Api.elm)

run-tests: Api.elm
	elm-test
