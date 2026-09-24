The example program from the README compiles to a stable blueprint.

  $ cat > bounce.combc << EOF
  > #PRIMARY GREEN
  > 
  > // bounce light back and forth at varying rates
  > 
  > int n = 10;
  > int t = 100;
  > 
  > signal ctr-out = signal-0;
  > signal rate-out = R;
  > int r = 7 * t;
  > 
  > // increase the rate over time
  > circuit div : rate-out = rate-out / t + 1;
  > circuit rate = counter(r - 1, rate-out) @ div;
  > 
  > // count up, to be used to determine which lamp is on
  > circuit add : ctr-out = ctr-out + 1;
  > circuit ctr = counter2(t * 2 - 1, rate-out, ctr-out) @ add;
  > 
  > // a line of n lamps
  > circuit lamps = for_concat i=0 to (n - 1) {
  >     condition cnd = ctr-out == i;
  >     output lamp(cnd) at (i, 0);
  > };
  > 
  > int t_by_n = t / n;
  > 
  > // first half of counter, light up from left, second half light up from right
  > circuit cmpt : ctr-out = if ctr-out <= t then
  >                             ctr-out / t_by_n
  >                         else
  >                         -(ctr-out / t_by_n - 2 * (n - 1));
  > 
  > output rate @ ctr @ cmpt;
  > output lamps at (0, 8);
  > EOF

  $ combc --output-json bounce.combc
  
  {
    "blueprint": {
      "entities": [
        {
          "entity_number": 1,
          "name": "substation",
          "position": { "x": 10.0, "y": 2.0 },
          "connections": {
            "1": { "green": [ { "entity_id": 16, "circuit_id": 2 } ] }
          }
        },
        {
          "entity_number": 2,
          "name": "substation",
          "position": { "x": -1.0, "y": 2.0 },
          "connections": {}
        },
        {
          "entity_number": 3,
          "name": "constant-combinator",
          "position": { "x": 0.5, "y": 0.5 },
          "control_behavior": {
            "filters": [
              {
                "signal": { "type": "virtual", "name": "signal-R" },
                "count": 1,
                "index": 1
              }
            ]
          },
          "connections": {
            "1": { "green": [ { "entity_id": 4, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 4,
          "name": "decider-combinator",
          "position": { "x": 1.5, "y": 1.0 },
          "control_behavior": {
            "decider_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-R" },
              "constant": 699,
              "comparator": "<",
              "output_signal": { "type": "virtual", "name": "signal-R" }
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 3, "circuit_id": 1 },
                { "entity_id": 4, "circuit_id": 2 }
              ]
            },
            "2": {
              "green": [
                { "entity_id": 4, "circuit_id": 1 },
                { "entity_id": 5, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 5,
          "name": "arithmetic-combinator",
          "position": { "x": 2.5, "y": 1.0 },
          "control_behavior": {
            "arithmetic_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-R" },
              "second_constant": 100,
              "operation": "/",
              "output_signal": { "type": "virtual", "name": "signal-0" }
            }
          },
          "connections": {
            "1": { "green": [ { "entity_id": 4, "circuit_id": 2 } ] },
            "2": { "green": [ { "entity_id": 6, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 6,
          "name": "arithmetic-combinator",
          "position": { "x": 3.5, "y": 1.0 },
          "control_behavior": {
            "arithmetic_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "second_constant": 1,
              "operation": "+",
              "output_signal": { "type": "virtual", "name": "signal-R" }
            }
          },
          "connections": {
            "1": { "green": [ { "entity_id": 5, "circuit_id": 2 } ] },
            "2": { "green": [ { "entity_id": 7, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 7,
          "name": "decider-combinator",
          "position": { "x": 4.5, "y": 1.0 },
          "control_behavior": {
            "decider_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-R" },
              "constant": 199,
              "comparator": "<",
              "output_signal": { "type": "virtual", "name": "signal-R" }
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 7, "circuit_id": 2 },
                { "entity_id": 6, "circuit_id": 2 }
              ]
            },
            "2": {
              "green": [
                { "entity_id": 7, "circuit_id": 1 },
                { "entity_id": 8, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 8,
          "name": "arithmetic-combinator",
          "position": { "x": 5.5, "y": 1.0 },
          "control_behavior": {
            "arithmetic_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-R" },
              "second_constant": 0,
              "operation": "+",
              "output_signal": { "type": "virtual", "name": "signal-0" }
            }
          },
          "connections": {
            "1": { "green": [ { "entity_id": 7, "circuit_id": 2 } ] },
            "2": { "green": [ { "entity_id": 9, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 9,
          "name": "arithmetic-combinator",
          "position": { "x": 6.5, "y": 1.0 },
          "control_behavior": {
            "arithmetic_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "second_constant": 1,
              "operation": "+",
              "output_signal": { "type": "virtual", "name": "signal-0" }
            }
          },
          "connections": {
            "1": { "green": [ { "entity_id": 8, "circuit_id": 2 } ] },
            "2": {
              "green": [
                { "entity_id": 15, "circuit_id": 1 },
                { "entity_id": 14, "circuit_id": 1 },
                { "entity_id": 13, "circuit_id": 1 },
                { "entity_id": 12, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 10,
          "name": "decider-combinator",
          "position": { "x": 7.5, "y": 1.0 },
          "control_behavior": {
            "decider_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-2" },
              "constant": 0,
              "comparator": "=",
              "output_signal": { "type": "virtual", "name": "signal-1" }
            }
          },
          "connections": {
            "1": {
              "red": [ { "entity_id": 12, "circuit_id": 2 } ],
              "green": [ { "entity_id": 15, "circuit_id": 2 } ]
            },
            "2": { "green": [ { "entity_id": 16, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 11,
          "name": "decider-combinator",
          "position": { "x": 8.5, "y": 1.0 },
          "control_behavior": {
            "decider_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-2" },
              "constant": 0,
              "comparator": "≠",
              "output_signal": { "type": "virtual", "name": "signal-1" }
            }
          },
          "connections": {
            "1": {
              "red": [ { "entity_id": 14, "circuit_id": 2 } ],
              "green": [ { "entity_id": 15, "circuit_id": 2 } ]
            },
            "2": { "green": [ { "entity_id": 16, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 12,
          "name": "arithmetic-combinator",
          "position": { "x": 0.5, "y": 3.0 },
          "control_behavior": {
            "arithmetic_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-3" },
              "second_constant": -1,
              "operation": "*",
              "output_signal": { "type": "virtual", "name": "signal-1" }
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 17, "circuit_id": 2 },
                { "entity_id": 9, "circuit_id": 2 }
              ]
            },
            "2": { "red": [ { "entity_id": 10, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 13,
          "name": "arithmetic-combinator",
          "position": { "x": 1.5, "y": 3.0 },
          "control_behavior": {
            "arithmetic_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "second_constant": 10,
              "operation": "/",
              "output_signal": { "type": "virtual", "name": "signal-4" }
            }
          },
          "connections": {
            "1": { "green": [ { "entity_id": 9, "circuit_id": 2 } ] },
            "2": { "green": [ { "entity_id": 17, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 14,
          "name": "arithmetic-combinator",
          "position": { "x": 2.5, "y": 3.0 },
          "control_behavior": {
            "arithmetic_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "second_constant": 10,
              "operation": "/",
              "output_signal": { "type": "virtual", "name": "signal-1" }
            }
          },
          "connections": {
            "1": { "green": [ { "entity_id": 9, "circuit_id": 2 } ] },
            "2": { "red": [ { "entity_id": 11, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 15,
          "name": "decider-combinator",
          "position": { "x": 3.5, "y": 3.0 },
          "control_behavior": {
            "decider_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 100,
              "comparator": "≤",
              "output_signal": { "type": "virtual", "name": "signal-2" },
              "copy_count_from_input": false
            }
          },
          "connections": {
            "1": { "green": [ { "entity_id": 9, "circuit_id": 2 } ] },
            "2": {
              "green": [
                { "entity_id": 11, "circuit_id": 1 },
                { "entity_id": 10, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 16,
          "name": "arithmetic-combinator",
          "position": { "x": 4.5, "y": 3.0 },
          "control_behavior": {
            "arithmetic_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-1" },
              "second_constant": 0,
              "operation": "+",
              "output_signal": { "type": "virtual", "name": "signal-0" }
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 11, "circuit_id": 2 },
                { "entity_id": 10, "circuit_id": 2 }
              ]
            },
            "2": { "green": [ { "entity_id": 1, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 17,
          "name": "arithmetic-combinator",
          "position": { "x": 5.5, "y": 3.0 },
          "control_behavior": {
            "arithmetic_conditions": {
              "first_signal": { "type": "virtual", "name": "signal-4" },
              "second_constant": 18,
              "operation": "-",
              "output_signal": { "type": "virtual", "name": "signal-3" }
            }
          },
          "connections": {
            "1": { "green": [ { "entity_id": 13, "circuit_id": 2 } ] },
            "2": { "green": [ { "entity_id": 12, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 18,
          "name": "substation",
          "position": { "x": 11.0, "y": 8.0 },
          "connections": {
            "1": { "green": [ { "entity_id": 30, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 19,
          "name": "substation",
          "position": { "x": -1.0, "y": 8.0 },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 21, "circuit_id": 1 },
                { "entity_id": 20, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 20,
          "name": "constant-combinator",
          "position": { "x": -1.0, "y": 9.5 },
          "control_behavior": {
            "filters": [
              {
                "signal": { "type": "virtual", "name": "signal-0" },
                "count": 1,
                "index": 1
              }
            ]
          },
          "connections": {
            "1": { "green": [ { "entity_id": 19, "circuit_id": 1 } ] }
          }
        },
        {
          "entity_number": 21,
          "name": "small-lamp",
          "position": { "x": 0.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 0,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 19, "circuit_id": 1 },
                { "entity_id": 22, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 22,
          "name": "small-lamp",
          "position": { "x": 1.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 1,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 23, "circuit_id": 1 },
                { "entity_id": 21, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 23,
          "name": "small-lamp",
          "position": { "x": 2.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 2,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 24, "circuit_id": 1 },
                { "entity_id": 22, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 24,
          "name": "small-lamp",
          "position": { "x": 3.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 3,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 25, "circuit_id": 1 },
                { "entity_id": 23, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 25,
          "name": "small-lamp",
          "position": { "x": 4.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 4,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 26, "circuit_id": 1 },
                { "entity_id": 24, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 26,
          "name": "small-lamp",
          "position": { "x": 5.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 5,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 27, "circuit_id": 1 },
                { "entity_id": 25, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 27,
          "name": "small-lamp",
          "position": { "x": 6.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 6,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 28, "circuit_id": 1 },
                { "entity_id": 26, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 28,
          "name": "small-lamp",
          "position": { "x": 7.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 7,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 29, "circuit_id": 1 },
                { "entity_id": 27, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 29,
          "name": "small-lamp",
          "position": { "x": 8.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 8,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 30, "circuit_id": 1 },
                { "entity_id": 28, "circuit_id": 1 }
              ]
            }
          }
        },
        {
          "entity_number": 30,
          "name": "small-lamp",
          "position": { "x": 9.0, "y": 8.0 },
          "control_behavior": {
            "circuit_condition": {
              "first_signal": { "type": "virtual", "name": "signal-0" },
              "constant": 9,
              "comparator": "="
            }
          },
          "connections": {
            "1": {
              "green": [
                { "entity_id": 18, "circuit_id": 1 },
                { "entity_id": 29, "circuit_id": 1 }
              ]
            }
          }
        }
      ],
      "item": "blueprint",
      "label": "bounce.combc"
    }
  }
