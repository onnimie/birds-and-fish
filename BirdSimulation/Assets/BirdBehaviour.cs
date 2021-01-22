using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BirdBehaviour : MonoBehaviour
{
    public BirdValues birdValues;
    float xBound = 60f;
    float yBound = 24f;

    // Start is called before the first frame update
    void Start()
    {
        birdValues = GameObject.Find("BirdGod").GetComponent<BirdValues>();
        
    }

    // Update is called once per frame
    void Update()
    {
        transform.Translate(0, birdValues.baseSpeed, 0);

        ReactOnOutOfBounds();
    }

    private void ReactOnOutOfBounds()
    {
        if (transform.position.x < -xBound || transform.position.x > xBound)
        {
            transform.position = new Vector3(-1 * transform.position.x, transform.position.y, 0);
        }
        if (transform.position.y < -yBound || transform.position.y > yBound)
        {
            transform.position = new Vector3(transform.position.x, -1 * transform.position.y, 0);
        }
    }
}
