using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SimulationMaster : MonoBehaviour
{
    public GameObject[] birdsAndFish = new GameObject[4];
    public GameObject creatureHolder;
    float xBound = 55f;
    float yBound = 20f;
    public int creatureAmount;
    [Range(0f, 100f)]
    public float fishPercent = 50f;

    private void Start()
    {
        SummonCreatures(creatureAmount, fishPercent);
    }

    private Color GetColourVariationFor(string objectTag)
    {
        if (objectTag == "bird")
        {
            int red = (int)Mathf.Round(Random.Range(0f, 230f));
            int green = (int)Mathf.Round(Random.Range(200f, 255f));
            int blue = (int)Mathf.Round(Random.Range(200f, 255f));

            return new Color(red, green, blue);
        }
        else
        {
            int red = (int)Mathf.Round(Random.Range(190f, 255f));
            int green = (int)Mathf.Round(Random.Range(175f, 255f));
            int blue = (int)Mathf.Round(Random.Range(245f, 255f));

            return new Color(red, green, blue);
        }
    }

    private Vector3 GetRandomPosition()
    {
        var x = Random.Range(-xBound, xBound);
        var y = Random.Range(-yBound, yBound);

        return new Vector3(x, y, 0);
    }

    private Quaternion GetRandomRotation()
    {
        return Quaternion.Euler(0, 0, Random.Range(0f, 360f));
    }

    private void Spawn(GameObject creature)
    {
        Instantiate(creature, GetRandomPosition(), GetRandomRotation(), creatureHolder.GetComponent<Transform>());
    }

    private void SummonCreatures(int totalAmount, float fishPercent)
    {
        int fishAmount = Mathf.RoundToInt(totalAmount * (fishPercent / 100));
        int birdAmount = totalAmount - fishAmount;

        for (int i = 0; i < fishAmount; i++)
        {
            if (i % 2 == 0)
            {
                Spawn(birdsAndFish[3]);
            } else {
                Spawn(birdsAndFish[2]);
            }
        }
        for (int i = 0; i < birdAmount; i++)
        {
            if (i % 2 == 0)
            {
                Spawn(birdsAndFish[0]);
            } else
            {
                Spawn(birdsAndFish[1]);
            }
        }

        foreach (SpriteRenderer creatureSprite in creatureHolder.transform.GetComponentsInChildren<SpriteRenderer>())
        {
            creatureSprite.color = GetColourVariationFor(creatureSprite.gameObject.tag);
        }
    }

    
}
