import openai
import datetime
import time
import os
import pandas as pd

openai.api_key = "my_key" # here you have to add your personal OpenAI API key

# list all authors (for GPT-3 prompt)
my_authors = ["Rhoda Broughton", "Charles Dickens", "Benjamin Disraeli", "George Eliot", "Edith Nesbit", "Ouida", "Anthony Trollope", "Mrs. Humphry Ward", "Herbert George Wells", "Charlotte Mary Yonge"]
# list just surnames (to save files)
my_surnames = ["Broughton", "Dickens", "Disraeli", "Eliot", "Nesbit", "Ouida", "Trollope", "Ward", "Wells", "Yonge"]

# read analysis features
df = pd.read_csv("gpt3_features.csv")

# main loop on all configurations
for my_try in range(0, len(df)):

  # verify if dir already exists (so files were already generated)
  if os.path.exists("GPT3/"+df["directory"][my_try]):
    print(df["directory"][my_try], "already done\n")
  
  else:
    # define try
    my_folder = "GPT3/"+df["directory"][my_try]

    # define prompt
    my_prompt = df["prompt"][my_try]

    # model features
    my_model = df["model"][my_try]
    my_temperature = df["temperature"][my_try]

    print("\nStart processing:", my_folder, "\nprompt:", my_prompt, "\nmodel:", my_model, "\ntemperature:", my_temperature, "\n")

    os.mkdir(my_folder)

    # secondary loop on all authors
    for i in range(0, len(my_authors)):

      my_author = my_authors[i]

      # create input
      my_input = my_prompt.replace("<my_author>", my_author)

      full_length = 0

      while full_length < 6000:  

        # create a completion
        completion = openai.Completion.create(engine=my_model, prompt=my_input, max_tokens=2000, temperature = my_temperature)

        # store the completion
        result = completion.choices[0].text

        # write file
        now = datetime.datetime.now()
        now = now.strftime("%Y%m%d%H%M%S")

        my_filename = my_folder+"/"+my_surnames[i]+"_"+str(now)+".txt"
        with open(my_filename, "w") as f:
          f.write(result)

        # calculate full length
        full_length += completion.usage.completion_tokens
        
        print(my_surnames[i], completion.usage.completion_tokens, full_length)

        # wait a second
        time.sleep(5)
