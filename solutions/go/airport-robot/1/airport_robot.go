package airportrobot

type Greeter interface {
	LanguageName() string
	Greet(visitorName string) string
}

func SayHello(visitorName string, greeter Greeter) string {
	return "I can speak " + greeter.LanguageName() + ": " +
		greeter.Greet(visitorName)
}

type Italian struct{}

func (Italian) LanguageName() string {
	return "Italian"
}

func (Italian) Greet(visitorName string) string {
	return "Ciao " + visitorName + "!"
}

type Portuguese struct{}

func (Portuguese) LanguageName() string {
	return "Portuguese"
}

func (Portuguese) Greet(visitorName string) string {
	return "Olá " + visitorName + "!"
}
