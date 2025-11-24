package phonenumber

import (
	"errors"
	"fmt"
	"regexp"
)

var nanp = regexp.MustCompile(
	`^[^\d]*1?[^\d]*` +
		`([2-9]\d\d)` /* $1 area code */ +
		`[^\d]*` +
		`([2-9]\d\d)` /* $2 exchange code */ +
		`[^\d]*` +
		`(\d{4})` /* $3 subscriber number */ +
		`[^\d]*$`,
)

func Number(phoneNumber string) (string, error) {
	match := nanp.FindStringSubmatch(phoneNumber)
	if match == nil {
		return "", errors.New("no match")
	}
	areaCode := match[1]
	exchangeCode := match[2]
	subscriberNumber := match[3]
	return fmt.Sprint(areaCode, exchangeCode, subscriberNumber), nil
}

func AreaCode(phoneNumber string) (string, error) {
	match := nanp.FindStringSubmatch(phoneNumber)
	if match == nil {
		return "", errors.New("no match")
	}
	return match[1], nil
}

func Format(phoneNumber string) (string, error) {
	match := nanp.FindStringSubmatch(phoneNumber)
	if match == nil {
		return "", errors.New("no match")
	}
	areaCode := match[1]
	exchangeCode := match[2]
	subscriberNumber := match[3]
	result := fmt.Sprintf(
		"(%s) %s-%s", areaCode, exchangeCode, subscriberNumber,
	)
	return result, nil
}
