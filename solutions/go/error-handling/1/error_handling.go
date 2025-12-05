package erratum

func Use(opener ResourceOpener, input string) (err error) {
	var resource Resource = nil
	for resource == nil {
		resource, err = opener()
		if _, isTransient := err.(TransientError); isTransient {
			resource = nil
			continue
		}
		if err != nil {
			return
		}
	}
	defer resource.Close()

	defer func() {
		if r := recover(); r != nil {
			switch r := r.(type) {
			case FrobError:
				resource.Defrob(r.defrobTag)
				err = r.inner
			case error:
				err = r
			default:
				panic(r)
			}
		}
	}()
	resource.Frob(input)

	return nil
}
