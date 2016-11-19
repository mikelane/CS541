"""
CS510 SLA Homework 3
Nandini Khanwalkar
Mike Lane
11/16/16
"""
from __future__ import print_function

import sqlite3

conn = sqlite3.connect(':memory:')
flight_status_data = [
    ('ajaxair', '113', 'portland', '08:03', 'atlanta', '12:51', 'landed', ''),
    ('ajaxair', '114', 'atlanta', '14:05', 'portland', '16:44', 'boarding', ''),
    ('bakerair', '121', 'atlanta', '17:14', 'new york city', '19:20', 'departed', ''),
    ('bakerair', '122', 'new york city', '21:00', 'portland', '00:13', 'scheduled', ''),
    ('bakerair', '124', 'portland', '09:03', 'atlanta', '12:52', 'delayed', '09:55'),
    ('carsonair', '522', 'portland', '14:15', 'new york city', '16:58', 'scheduled', ''),
    ('carsonair', '679', 'new york city', '09:30', 'atlanta', '11:30', 'departed', ''),
    ('carsonair', '670', 'new york city', '09:30', 'portland', '12:05', 'departed', ''),
    ('carsonair', '671', 'atlanta', '13:20', 'new york city', '14:55', 'scheduled', ''),
    ('carsonair', '672', 'portland', '13:25', 'new york city', '20:36', 'scheduled', '')]

valid_data = {
    'airlines': {'ajaxair', 'bakerair', 'carsonair'},
    'cities': {'portland', 'atlanta', 'new york city'}
}

c = conn.cursor()
c.execute('''CREATE TABLE flightstatus (
    airline TEXT,
    flight_number INTEGER,
    departure_city TEXT,
    departure_time TEXT,
    arrival_city TEXT,
    arrival_time TEXT,
    status TEXT,
    delay_time TEXT,
    PRIMARY KEY(airline, flight_number));''')
conn.commit()
c.executemany('INSERT INTO flightstatus VALUES(?,?,?,?,?,?,?,?)', flight_status_data)


# --------------- Helpers that build all of the responses ----------------------

def build_speechlet_response(title, output, reprompt_text, should_end_session):
    return {
        'outputSpeech': {
            'type': 'PlainText',
            'text': output
        },
        'card': {
            'type': 'Simple',
            'title': "SessionSpeechlet - " + title,
            'content': "SessionSpeechlet - " + output
        },
        'reprompt': {
            'outputSpeech': {
                'type': 'PlainText',
                'text': reprompt_text
            }
        },
        'shouldEndSession': should_end_session
    }


def build_response(session_attributes, speechlet_response):
    return {
        'version': '1.0',
        'sessionAttributes': session_attributes,
        'response': speechlet_response
    }


# --------------- Functions that control the skill's behavior ------------------

def get_welcome_response():
    """ If we wanted to initialize the session to have some attributes we could
    add those here
    """

    session_attributes = {}

    card_title = "Welcome"
    speech_output = "Welcome to the CS510 Flight Finder. Find the status of a" \
                    "flight by saying something like, what is the status of ajax air one one" \
                    "three"
    # If the user either does not reply to the welcome message or says something
    # that is not understood, they will be prompted again with this text.
    reprompt_text = "Find the status of a flight by saying something" \
                    "like, what is the status of ajax air one one three"
    should_end_session = False
    return build_response(session_attributes, build_speechlet_response(
        card_title, speech_output, reprompt_text, should_end_session))


def handle_session_end_request():
    card_title = "Session Ended"
    speech_output = "Thank you for trying the CS510 flight finder. " \
                    "Have a nice day! "
    # Setting this to true ends the session and exits the skill.
    should_end_session = True
    return build_response({}, build_speechlet_response(
        card_title, speech_output, None, should_end_session))


def get_flight_status(intent, session):
    """ Gets the flight status and prepares the speech to reply to the user."""

    card_title = intent['name']
    session_attributes = {}
    should_end_session = True
    statuses = []
    reprompt_text = None

    try:
        airline = ''.join(intent['slots']['Airline']['value'].lower().split())
        if airline and 'jack' in airline:
            airline = 'ajaxair'

        if airline and 'air' not in airline:
            airline += 'air'

        if airline not in valid_data['airlines']:
            speech_output = "I can only find the status of ajax air, baker air and carson air"
            return build_response(session_attributes, build_speechlet_response(
                card_title, speech_output, reprompt_text, should_end_session))
    except KeyError:
        airline = None

    try:
        flight_number = int(intent['slots']['FlightNumber']['value'])
    except (ValueError, KeyError) as e:
        if type(e).__name__ == 'KeyError':
            flight_number = None
        elif type(e).__name__ == 'ValueError':
            speech_output = "I need a number for the flight number value."
            return build_response(session_attributes, build_speechlet_response(
                card_title, speech_output, reprompt_text, should_end_session))

    if airline and flight_number:
        c.execute('''
        SELECT airline, flight_number, status
        FROM flightstatus
        WHERE airline=?
        AND flight_number=?
        ''', (airline, flight_number))
        results = c.fetchall()
        if results:
            for row in results:
                statuses.append('{} flight {} has status {}'.format(*row))
                speech_output = 'I found the following status, {}'.format(', '.join(statuses))
        else:
            speech_output = "I didn't find any flight status for {} {}".format(
                airline, flight_number)
    elif flight_number:
        c.execute('''
        SELECT airline, flight_number, status
        FROM flightstatus
        WHERE flight_number=?
        ''', (flight_number,))
        results = c.fetchall()
        if results:
            for row in results:
                statuses.append('{} flight {} has status {}'.format(*row))
                speech_output = 'I found the following statuses, ' + ', '.join(statuses)
        else:
            speech_output = "I didn't find any flight statuses for flight {}".format(
                flight_number)
    elif airline:
        c.execute('''
        SELECT airline, flight_number, status
        FROM flightstatus
        WHERE airline=?
        ''', (airline,))
        results = c.fetchall()
        if results:
            for row in results:
                statuses.append('{} flight {} has status {}'.format(*row))
                speech_output = 'I found the following statuses, {}'.format(', '.join(statuses))
        else:
            speech_output = "I didn't find any flight statuses for airline {}".format(
                airline)
    else:
        speech_output = "I'm not sure what flight status to find. Please try again"
        reprompt_text = "I didn't understand. You can ask for a flight status" \
                        "by saying something like, what is the status of ajax air one one three."
        should_end_session = False

    return build_response(session_attributes, build_speechlet_response(
        card_title, speech_output, reprompt_text, should_end_session))


def get_flight_info(intent, session):
    card_title = intent['name']
    should_end_session = True
    session_attributes = {}
    info = []
    reprompt_text = None

    try:
        airline = ''.join(intent['slots']['Airline']['value'].lower().split())
        if airline and 'jack' in airline:
            airline = 'ajaxair'

        if airline and 'air' not in airline:
            airline += 'air'

        if airline not in valid_data['airlines']:
            speech_output = "I can only find the status of ajax air, baker air and carson air"
            return build_response(session_attributes, build_speechlet_response(
                card_title, speech_output, reprompt_text, should_end_session))
    except KeyError:
        airline = None

    try:
        flight_number = int(intent['slots']['FlightNumber']['value'])
    except (ValueError, KeyError) as e:
        if type(e).__name__ == 'KeyError':
            flight_number = None
        elif type(e).__name__ == 'ValueError':
            speech_output = "I need a number for the flight number value."
            return build_response(session_attributes, build_speechlet_response(
                card_title, speech_output, reprompt_text, should_end_session))
    try:
        source = intent['slots']['Source']['value']
        source = ''.join(source.lower().split())
        if source in ('nyc', 'newyork', 'newyorkcity'):
            source = 'new york city'
        elif source in ('pdx'):
            source = 'portland'
        elif source in ('atl'):
            source = 'atlanta'
        else:
            source = "invalid"
    except KeyError:
        source = None

    try:
        destination = intent['slots']['Destination']['value']
        destination = ''.join(destination.lower().split())
        if destination in ('nyc', 'newyork', 'newyorkcity'):
            destination = 'new york city'
        elif destination in ('pdx'):
            destination = 'portland'
        elif destination in ('atl'):
            destination = 'atlanta'
        else:
            destination = "invalid"
    except KeyError:
        destination = None

    if airline and flight_number:
        c.execute('''
	    SELECT * FROM flightstatus
	    WHERE airline=?
	    AND flight_number=?
	    ''', (airline, flight_number))
        results = c.fetchall()
        if results:
            for row in results:
                info.append('{} {} departs {} at {} and arrives in {} at {}. Its current status is {}'.format(*row))
                speech_output = "I found the following flight info, " + ', '.join(info)
        else:
            speech_output = "I didn't find any flight information for {} flight {}".format(airline, flight_number)
    elif airline:
        c.execute('''
        SELECT *
        FROM flightstatus
        WHERE airline=?''', (airline,))
        results = c.fetchall()
        if results:
            for row in results:
                info.append("{} {} departs {} at {} and arrives in {} at {}, its current status is {}".format(*row))
                speech_output = "The flight info I found is, " + '. '.join(info)
        else:
            speech_output = "I didn't find any {} flights".format(airline)

    elif flight_number:
        c.execute('''
        SELECT *
        FROM flightstatus
        WHERE flight_number=?''', (flight_number,))
        results = c.fetchall()
        if results:
            for row in results:
                info.append("{} {} departs {} at {} and arrives in {} at {}, its current status is {}".format(*row))
                speech_output = "I found the following flight info, " + '. '.join(info)
        else:
            speech_output = "I didn't find flight number {}".format(flight_number)
    elif source == 'invalid':
        speech_output = "I don't know anything about {}. I can only find flight info for Portland, Atlanta, and New York City."
    elif destination == 'invalid':
        speech_output = "I don't know anything about {}. I can only find flight info for Portland, Atlanta, and New York City."
    elif source and destination:
        c.execute('''
        SELECT *
        FROM flightstatus
        WHERE departure_city=?
        AND arrival_city=?
        ''', (source, destination))
        results = c.fetchall()
        if results:
            for row in results:
                info.append('{} {} departs {} at {} and arrives in {} at {}, its current status is {}.'.format(*row))
                speech_output = "Here's what I found. ".format(source, destination) + '. '.join(info)
        else:
            speech_output = "I didn't find any flights between {} and {}".format(source, destination)
    elif source:
        c.execute('''
        SELECT airline, flight_number, departure_time, arrival_city, arrival_time, status
        FROM flightstatus
        WHERE departure_city=?
        ''', (source,))
        results = c.fetchall()
        if results:
            for row in results:
                info.append('{} {} departs at {} and arrives in {} at {}, its current status is {}'.format(*row))
                speech_output = "The flights out of {} are, ".format(source) + '. '.join(info)
        else:
            speech_output = "I didn't find any flights out of {}".format(source)
    elif destination:
        c.execute('''
        SELECT airline, flight_number, departure_city, departure_time, arrival_time, status
        FROM flightstatus
        WHERE arrival_city=?''', (destination,))
        results = c.fetchall()
        if results:
            for row in results:
                info.append('{} {} departs {} at {} and arrives at {}. Its current status is {}'.format(*row))
                speech_output = "The flights into {} are, ".format(destination) + '. '.join(info)
        else:
            speech_output = "I didn't find any flights into {}".format(destination)
    else:
        speech_output = "I'm not sure what flight information to find. Please try again."
        reprompt_text = "I didn't understand. You can ask for a flight information " \
                        "by saying something like, find me a flight from Portland to New York, " \
                        "or something like, find flight AjaxAir one one three "
    return build_response(session_attributes, build_speechlet_response(card_title,
                                                                       speech_output, reprompt_text, should_end_session))


# --------------- Events ------------------

def on_session_started(session_started_request, session):
    """ Called when the session starts """

    print("on_session_started requestId=" + session_started_request['requestId']
          + ", sessionId=" + session['sessionId'])


def on_launch(launch_request, session):
    """ Called when the user launches the skill without specifying what they
    want
    """

    print("on_launch requestId=" + launch_request['requestId'] +
          ", sessionId=" + session['sessionId'])
    # Dispatch to your skill's launch
    return get_welcome_response()


def on_intent(intent_request, session):
    """ Called when the user specifies an intent for this skill """

    print("on_intent requestId=" + intent_request['requestId'] +
          ", sessionId=" + session['sessionId'])

    intent = intent_request['intent']
    intent_name = intent_request['intent']['name']

    # Dispatch to your skill's intent handlers
    print(intent_name)
    if intent_name == "FindFlightIntent":
        return get_flight_info(intent, session)
    elif intent_name == "FindFlightStatusIntent":
        return get_flight_status(intent, session)
    elif intent_name == "AMAZON.HelpIntent":
        return get_welcome_response()
    elif intent_name == "AMAZON.CancelIntent" or intent_name == "AMAZON.StopIntent":
        return handle_session_end_request()
    else:
        raise ValueError("Invalid intent")


def on_session_ended(session_ended_request, session):
    """ Called when the user ends the session.
    Is not called when the skill returns should_end_session=true
    """
    print("on_session_ended requestId=" + session_ended_request['requestId'] +
          ", sessionId=" + session['sessionId'])
    # add cleanup logic here


# --------------- Main handler ------------------

def lambda_handler(event, context):
    """ Route the incoming request based on type (LaunchRequest, IntentRequest,
    etc.) The JSON body of the request is provided in the event parameter.
    """
    print("event.session.application.applicationId=" +
          event['session']['application']['applicationId'])

    """
    Uncomment this if statement and populate with your skill's application ID to
    prevent someone else from configuring a skill that sends requests to this
    function.
    """
    # if (event['session']['application']['applicationId'] !=
    #         "amzn1.ask.skill.8e1acab1-5dff-40c9-a44e-7d82b2af2678"):
    #     raise ValueError("Invalid Application ID")

    if event['session']['new']:
        on_session_started({'requestId': event['request']['requestId']},
                           event['session'])

    if event['request']['type'] == "LaunchRequest":
        return on_launch(event['request'], event['session'])
    elif event['request']['type'] == "IntentRequest":
        return on_intent(event['request'], event['session'])
    elif event['request']['type'] == "SessionEndedRequest":
        return on_session_ended(event['request'], event['session'])
